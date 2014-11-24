{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters. For information about managing security
-- groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html
-- Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster
-- Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSecurityGroup.html>
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Request
      CreateClusterSecurityGroup
    -- ** Request constructor
    , createClusterSecurityGroup
    -- ** Request lenses
    , ccsg1ClusterSecurityGroupName
    , ccsg1Description
    , ccsg1Tags

    -- * Response
    , CreateClusterSecurityGroupResponse
    -- ** Response constructor
    , createClusterSecurityGroupResponse
    -- ** Response lenses
    , ccsgrClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateClusterSecurityGroup = CreateClusterSecurityGroup
    { _ccsg1ClusterSecurityGroupName :: Text
    , _ccsg1Description              :: Text
    , _ccsg1Tags                     :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateClusterSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsg1ClusterSecurityGroupName' @::@ 'Text'
--
-- * 'ccsg1Description' @::@ 'Text'
--
-- * 'ccsg1Tags' @::@ ['Tag']
--
createClusterSecurityGroup :: Text -- ^ 'ccsg1ClusterSecurityGroupName'
                           -> Text -- ^ 'ccsg1Description'
                           -> CreateClusterSecurityGroup
createClusterSecurityGroup p1 p2 = CreateClusterSecurityGroup
    { _ccsg1ClusterSecurityGroupName = p1
    , _ccsg1Description              = p2
    , _ccsg1Tags                     = mempty
    }

-- | The name for the security group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all
-- security groups that are created by your AWS account. Example:
-- 'examplesecuritygroup'.
ccsg1ClusterSecurityGroupName :: Lens' CreateClusterSecurityGroup Text
ccsg1ClusterSecurityGroupName =
    lens _ccsg1ClusterSecurityGroupName
        (\s a -> s { _ccsg1ClusterSecurityGroupName = a })

-- | A description for the security group.
ccsg1Description :: Lens' CreateClusterSecurityGroup Text
ccsg1Description = lens _ccsg1Description (\s a -> s { _ccsg1Description = a })

-- | A list of tag instances.
ccsg1Tags :: Lens' CreateClusterSecurityGroup [Tag]
ccsg1Tags = lens _ccsg1Tags (\s a -> s { _ccsg1Tags = a }) . _List

newtype CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse
    { _ccsgrClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show)

-- | 'CreateClusterSecurityGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
createClusterSecurityGroupResponse :: CreateClusterSecurityGroupResponse
createClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse
    { _ccsgrClusterSecurityGroup = Nothing
    }

ccsgrClusterSecurityGroup :: Lens' CreateClusterSecurityGroupResponse (Maybe ClusterSecurityGroup)
ccsgrClusterSecurityGroup =
    lens _ccsgrClusterSecurityGroup
        (\s a -> s { _ccsgrClusterSecurityGroup = a })

instance ToPath CreateClusterSecurityGroup where
    toPath = const "/"

instance ToQuery CreateClusterSecurityGroup where
    toQuery CreateClusterSecurityGroup{..} = mconcat
        [ "ClusterSecurityGroupName" =? _ccsg1ClusterSecurityGroupName
        , "Description"              =? _ccsg1Description
        , "Tags"                     =? _ccsg1Tags
        ]

instance ToHeaders CreateClusterSecurityGroup

instance AWSRequest CreateClusterSecurityGroup where
    type Sv CreateClusterSecurityGroup = Redshift
    type Rs CreateClusterSecurityGroup = CreateClusterSecurityGroupResponse

    request  = post "CreateClusterSecurityGroup"
    response = xmlResponse

instance FromXML CreateClusterSecurityGroupResponse where
    parseXML = withElement "CreateClusterSecurityGroupResult" $ \x -> CreateClusterSecurityGroupResponse
        <$> x .@? "ClusterSecurityGroup"

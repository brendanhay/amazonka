{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide.
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Request
      CreateClusterSecurityGroup
    -- ** Request constructor
    , createClusterSecurityGroup
    -- ** Request lenses
    , ccsg1ClusterSecurityGroupName
    , ccsg1Description

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
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateClusterSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsg1ClusterSecurityGroupName' @::@ 'Text'
--
-- * 'ccsg1Description' @::@ 'Text'
--
createClusterSecurityGroup :: Text -- ^ 'ccsg1ClusterSecurityGroupName'
                           -> Text -- ^ 'ccsg1Description'
                           -> CreateClusterSecurityGroup
createClusterSecurityGroup p1 p2 = CreateClusterSecurityGroup
    { _ccsg1ClusterSecurityGroupName = p1
    , _ccsg1Description              = p2
    }

-- | The name for the security group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all
-- security groups that are created by your AWS account. Example:
-- examplesecuritygroup.
ccsg1ClusterSecurityGroupName :: Lens' CreateClusterSecurityGroup Text
ccsg1ClusterSecurityGroupName =
    lens _ccsg1ClusterSecurityGroupName
        (\s a -> s { _ccsg1ClusterSecurityGroupName = a })

-- | A description for the security group.
ccsg1Description :: Lens' CreateClusterSecurityGroup Text
ccsg1Description = lens _ccsg1Description (\s a -> s { _ccsg1Description = a })

newtype CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse
    { _ccsgrClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

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

instance AWSRequest CreateClusterSecurityGroup where
    type Sv CreateClusterSecurityGroup = Redshift
    type Rs CreateClusterSecurityGroup = CreateClusterSecurityGroupResponse

    request  = post "CreateClusterSecurityGroup"
    response = xmlResponse

instance FromXML CreateClusterSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateClusterSecurityGroupResponse"

instance ToPath CreateClusterSecurityGroup where
    toPath = const "/"

instance ToHeaders CreateClusterSecurityGroup

instance ToQuery CreateClusterSecurityGroup

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift subnet group. You must provide a list of one
-- or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC)
-- when creating Amazon Redshift subnet group. For information about subnet
-- groups, go to Amazon Redshift Cluster Subnet Groups in the Amazon Redshift
-- Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSubnetGroup.html>
module Network.AWS.Redshift.CreateClusterSubnetGroup
    (
    -- * Request
      CreateClusterSubnetGroup
    -- ** Request constructor
    , createClusterSubnetGroup
    -- ** Request lenses
    , ccsgClusterSubnetGroupName
    , ccsgDescription
    , ccsgSubnetIds

    -- * Response
    , CreateClusterSubnetGroupResponse
    -- ** Response constructor
    , createClusterSubnetGroupResponse
    -- ** Response lenses
    , ccsgrClusterSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateClusterSubnetGroup = CreateClusterSubnetGroup
    { _ccsgClusterSubnetGroupName :: Text
    , _ccsgDescription            :: Text
    , _ccsgSubnetIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateClusterSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgClusterSubnetGroupName' @::@ 'Text'
--
-- * 'ccsgDescription' @::@ 'Text'
--
-- * 'ccsgSubnetIds' @::@ ['Text']
--
createClusterSubnetGroup :: Text -- ^ 'ccsgClusterSubnetGroupName'
                         -> Text -- ^ 'ccsgDescription'
                         -> CreateClusterSubnetGroup
createClusterSubnetGroup p1 p2 = CreateClusterSubnetGroup
    { _ccsgClusterSubnetGroupName = p1
    , _ccsgDescription            = p2
    , _ccsgSubnetIds              = mempty
    }

-- | The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all
-- subnet groups that are created by your AWS account. Example:
-- examplesubnetgroup.
ccsgClusterSubnetGroupName :: Lens' CreateClusterSubnetGroup Text
ccsgClusterSubnetGroupName =
    lens _ccsgClusterSubnetGroupName
        (\s a -> s { _ccsgClusterSubnetGroupName = a })

-- | A description for the subnet group.
ccsgDescription :: Lens' CreateClusterSubnetGroup Text
ccsgDescription = lens _ccsgDescription (\s a -> s { _ccsgDescription = a })

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
ccsgSubnetIds :: Lens' CreateClusterSubnetGroup [Text]
ccsgSubnetIds = lens _ccsgSubnetIds (\s a -> s { _ccsgSubnetIds = a })

newtype CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { _ccsgrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateClusterSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrClusterSubnetGroup' @::@ 'Maybe' 'ClusterSubnetGroup'
--
createClusterSubnetGroupResponse :: CreateClusterSubnetGroupResponse
createClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { _ccsgrClusterSubnetGroup = Nothing
    }

ccsgrClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
ccsgrClusterSubnetGroup =
    lens _ccsgrClusterSubnetGroup (\s a -> s { _ccsgrClusterSubnetGroup = a })

instance ToPath CreateClusterSubnetGroup where
    toPath = const "/"

instance ToQuery CreateClusterSubnetGroup

instance ToHeaders CreateClusterSubnetGroup

instance AWSRequest CreateClusterSubnetGroup where
    type Sv CreateClusterSubnetGroup = Redshift
    type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse

    request  = post "CreateClusterSubnetGroup"
    response = xmlResponse

instance FromXML CreateClusterSubnetGroupResponse where
    parseXML = withElement "CreateClusterSubnetGroupResult" $ \x ->
        CreateClusterSubnetGroupResponse
            <$> x .@? "ClusterSubnetGroup"

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.Redshift.CreateClusterSubnetGroup
    (
    -- * Request
      CreateClusterSubnetGroupMessage
    -- ** Request constructor
    , createClusterSubnetGroupMessage
    -- ** Request lenses
    , ccsgm1ClusterSubnetGroupName
    , ccsgm1Description
    , ccsgm1SubnetIds

    -- * Response
    , CreateClusterSubnetGroupResult
    -- ** Response constructor
    , createClusterSubnetGroupResult
    -- ** Response lenses
    , ccsgrClusterSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateClusterSubnetGroupMessage = CreateClusterSubnetGroupMessage
    { _ccsgm1ClusterSubnetGroupName :: Text
    , _ccsgm1Description            :: Text
    , _ccsgm1SubnetIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateClusterSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgm1ClusterSubnetGroupName' @::@ 'Text'
--
-- * 'ccsgm1Description' @::@ 'Text'
--
-- * 'ccsgm1SubnetIds' @::@ ['Text']
--
createClusterSubnetGroupMessage :: Text -- ^ 'ccsgm1ClusterSubnetGroupName'
                                -> Text -- ^ 'ccsgm1Description'
                                -> CreateClusterSubnetGroupMessage
createClusterSubnetGroupMessage p1 p2 = CreateClusterSubnetGroupMessage
    { _ccsgm1ClusterSubnetGroupName = p1
    , _ccsgm1Description            = p2
    , _ccsgm1SubnetIds              = mempty
    }

-- | The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all
-- subnet groups that are created by your AWS account. Example:
-- examplesubnetgroup.
ccsgm1ClusterSubnetGroupName :: Lens' CreateClusterSubnetGroupMessage Text
ccsgm1ClusterSubnetGroupName =
    lens _ccsgm1ClusterSubnetGroupName
        (\s a -> s { _ccsgm1ClusterSubnetGroupName = a })

-- | A description for the subnet group.
ccsgm1Description :: Lens' CreateClusterSubnetGroupMessage Text
ccsgm1Description =
    lens _ccsgm1Description (\s a -> s { _ccsgm1Description = a })

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
ccsgm1SubnetIds :: Lens' CreateClusterSubnetGroupMessage [Text]
ccsgm1SubnetIds = lens _ccsgm1SubnetIds (\s a -> s { _ccsgm1SubnetIds = a })

instance ToPath CreateClusterSubnetGroupMessage where
    toPath = const "/"

instance ToQuery CreateClusterSubnetGroupMessage

newtype CreateClusterSubnetGroupResult = CreateClusterSubnetGroupResult
    { _ccsgrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateClusterSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrClusterSubnetGroup' @::@ 'Maybe' 'ClusterSubnetGroup'
--
createClusterSubnetGroupResult :: CreateClusterSubnetGroupResult
createClusterSubnetGroupResult = CreateClusterSubnetGroupResult
    { _ccsgrClusterSubnetGroup = Nothing
    }

ccsgrClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResult (Maybe ClusterSubnetGroup)
ccsgrClusterSubnetGroup =
    lens _ccsgrClusterSubnetGroup (\s a -> s { _ccsgrClusterSubnetGroup = a })

instance AWSRequest CreateClusterSubnetGroupMessage where
    type Sv CreateClusterSubnetGroupMessage = Redshift
    type Rs CreateClusterSubnetGroupMessage = CreateClusterSubnetGroupResult

    request  = post "CreateClusterSubnetGroup"
    response = const . xmlResponse $ \h x -> CreateClusterSubnetGroupResult
newtype

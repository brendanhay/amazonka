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

-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift parameter group. Creating parameter groups is
-- independent of creating clusters. You can associate a cluster with a
-- parameter group when you create the cluster. You can also associate an
-- existing cluster with a parameter group after the cluster is created by
-- using ModifyCluster. Parameters in the parameter group define specific
-- behavior that applies to the databases you create on the cluster. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.CreateClusterParameterGroup
    (
    -- * Request
      CreateClusterParameterGroupMessage
    -- ** Request constructor
    , createClusterParameterGroup
    -- ** Request lenses
    , ccpgmDescription
    , ccpgmParameterGroupFamily
    , ccpgmParameterGroupName

    -- * Response
    , CreateClusterParameterGroupResult
    -- ** Response constructor
    , createClusterParameterGroupResponse
    -- ** Response lenses
    , ccpgrClusterParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateClusterParameterGroupMessage = CreateClusterParameterGroupMessage
    { _ccpgmDescription          :: Text
    , _ccpgmParameterGroupFamily :: Text
    , _ccpgmParameterGroupName   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateClusterParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgmDescription' @::@ 'Text'
--
-- * 'ccpgmParameterGroupFamily' @::@ 'Text'
--
-- * 'ccpgmParameterGroupName' @::@ 'Text'
--
createClusterParameterGroup :: Text -- ^ 'ccpgmParameterGroupName'
                            -> Text -- ^ 'ccpgmParameterGroupFamily'
                            -> Text -- ^ 'ccpgmDescription'
                            -> CreateClusterParameterGroupMessage
createClusterParameterGroup p1 p2 p3 = CreateClusterParameterGroupMessage
    { _ccpgmParameterGroupName   = p1
    , _ccpgmParameterGroupFamily = p2
    , _ccpgmDescription          = p3
    }

-- | A description of the parameter group.
ccpgmDescription :: Lens' CreateClusterParameterGroupMessage Text
ccpgmDescription = lens _ccpgmDescription (\s a -> s { _ccpgmDescription = a })

-- | The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters. To
-- get a list of valid parameter group family names, you can call
-- DescribeClusterParameterGroups. By default, Amazon Redshift returns a
-- list of all the parameter groups that are owned by your AWS account,
-- including the default parameter groups for each Amazon Redshift engine
-- version. The parameter group family names associated with the default
-- parameter groups provide you the valid values. For example, a valid
-- family name is "redshift-1.0".
ccpgmParameterGroupFamily :: Lens' CreateClusterParameterGroupMessage Text
ccpgmParameterGroupFamily =
    lens _ccpgmParameterGroupFamily
        (\s a -> s { _ccpgmParameterGroupFamily = a })

-- | The name of the cluster parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters or hyphens First character must be a letter.
-- Cannot end with a hyphen or contain two consecutive hyphens. Must be
-- unique withing your AWS account.
ccpgmParameterGroupName :: Lens' CreateClusterParameterGroupMessage Text
ccpgmParameterGroupName =
    lens _ccpgmParameterGroupName (\s a -> s { _ccpgmParameterGroupName = a })

instance ToPath CreateClusterParameterGroupMessage where
    toPath = const "/"

instance ToQuery CreateClusterParameterGroupMessage

newtype CreateClusterParameterGroupResult = CreateClusterParameterGroupResult
    { _ccpgrClusterParameterGroup :: Maybe ClusterParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateClusterParameterGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrClusterParameterGroup' @::@ 'Maybe' 'ClusterParameterGroup'
--
createClusterParameterGroupResponse :: CreateClusterParameterGroupResult
createClusterParameterGroupResponse = CreateClusterParameterGroupResult
    { _ccpgrClusterParameterGroup = Nothing
    }

ccpgrClusterParameterGroup :: Lens' CreateClusterParameterGroupResult (Maybe ClusterParameterGroup)
ccpgrClusterParameterGroup =
    lens _ccpgrClusterParameterGroup
        (\s a -> s { _ccpgrClusterParameterGroup = a })

instance AWSRequest CreateClusterParameterGroupMessage where
    type Sv CreateClusterParameterGroupMessage = Redshift
    type Rs CreateClusterParameterGroupMessage = CreateClusterParameterGroupResult

    request  = post "CreateClusterParameterGroup"
    response = xmlResponse $ \h x -> CreateClusterParameterGroupResult
        <$> x %| "ClusterParameterGroup"

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

-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a placement group that you launch cluster instances into. You must
-- give the group a name that's unique within the scope of your account.
--
-- For more information about placement groups and cluster instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreatePlacementGroup.html>
module Network.AWS.EC2.CreatePlacementGroup
    (
    -- * Request
      CreatePlacementGroup
    -- ** Request constructor
    , createPlacementGroup
    -- ** Request lenses
    , cpgDryRun
    , cpgGroupName
    , cpgStrategy

    -- * Response
    , CreatePlacementGroupResponse
    -- ** Response constructor
    , createPlacementGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreatePlacementGroup = CreatePlacementGroup
    { _cpgDryRun    :: Maybe Bool
    , _cpgGroupName :: Text
    , _cpgStrategy  :: PlacementStrategy
    } deriving (Eq, Read, Show)

-- | 'CreatePlacementGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cpgGroupName' @::@ 'Text'
--
-- * 'cpgStrategy' @::@ 'PlacementStrategy'
--
createPlacementGroup :: Text -- ^ 'cpgGroupName'
                     -> PlacementStrategy -- ^ 'cpgStrategy'
                     -> CreatePlacementGroup
createPlacementGroup p1 p2 = CreatePlacementGroup
    { _cpgGroupName = p1
    , _cpgStrategy  = p2
    , _cpgDryRun    = Nothing
    }

cpgDryRun :: Lens' CreatePlacementGroup (Maybe Bool)
cpgDryRun = lens _cpgDryRun (\s a -> s { _cpgDryRun = a })

-- | A name for the placement group.
--
-- Constraints: Up to 255 ASCII characters
cpgGroupName :: Lens' CreatePlacementGroup Text
cpgGroupName = lens _cpgGroupName (\s a -> s { _cpgGroupName = a })

-- | The placement strategy.
cpgStrategy :: Lens' CreatePlacementGroup PlacementStrategy
cpgStrategy = lens _cpgStrategy (\s a -> s { _cpgStrategy = a })

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CreatePlacementGroupResponse' constructor.
createPlacementGroupResponse :: CreatePlacementGroupResponse
createPlacementGroupResponse = CreatePlacementGroupResponse

instance ToPath CreatePlacementGroup where
    toPath = const "/"

instance ToQuery CreatePlacementGroup where
    toQuery CreatePlacementGroup{..} = mconcat
        [ "DryRun"    =? _cpgDryRun
        , "GroupName" =? _cpgGroupName
        , "Strategy"  =? _cpgStrategy
        ]

instance ToHeaders CreatePlacementGroup

instance AWSRequest CreatePlacementGroup where
    type Sv CreatePlacementGroup = EC2
    type Rs CreatePlacementGroup = CreatePlacementGroupResponse

    request  = post "CreatePlacementGroup"
    response = nullResponse CreatePlacementGroupResponse

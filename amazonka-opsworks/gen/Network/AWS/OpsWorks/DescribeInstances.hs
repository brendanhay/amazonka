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

-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a set of instances. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.DescribeInstances
    (
    -- * Request
      DescribeInstances
    -- ** Request constructor
    , describeInstances
    -- ** Request lenses
    , diInstanceIds
    , diLayerId
    , diStackId

    -- * Response
    , DescribeInstancesResponse
    -- ** Response constructor
    , describeInstancesResponse
    -- ** Response lenses
    , dirInstances
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeInstances = DescribeInstances
    { _diInstanceIds :: [Text]
    , _diLayerId     :: Maybe Text
    , _diStackId     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diInstanceIds' @::@ ['Text']
--
-- * 'diLayerId' @::@ 'Maybe' 'Text'
--
-- * 'diStackId' @::@ 'Maybe' 'Text'
--
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { _diStackId     = Nothing
    , _diLayerId     = Nothing
    , _diInstanceIds = mempty
    }

-- | An array of instance IDs to be described. If you use this parameter,
-- DescribeInstances returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
diInstanceIds :: Lens' DescribeInstances [Text]
diInstanceIds = lens _diInstanceIds (\s a -> s { _diInstanceIds = a })

-- | A layer ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified layer.
diLayerId :: Lens' DescribeInstances (Maybe Text)
diLayerId = lens _diLayerId (\s a -> s { _diLayerId = a })

-- | A stack ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified stack.
diStackId :: Lens' DescribeInstances (Maybe Text)
diStackId = lens _diStackId (\s a -> s { _diStackId = a })

instance ToPath DescribeInstances where
    toPath = const "/"

instance ToQuery DescribeInstances where
    toQuery = const mempty

instance ToHeaders DescribeInstances

instance ToBody DescribeInstances where
    toBody = toBody . encode . _diStackId

newtype DescribeInstancesResponse = DescribeInstancesResponse
    { _dirInstances :: [Instance]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeInstancesResponse where
    type Item DescribeInstancesResponse = Instance

    fromList = DescribeInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dirInstances

-- | 'DescribeInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirInstances' @::@ ['Instance']
--
describeInstancesResponse :: DescribeInstancesResponse
describeInstancesResponse = DescribeInstancesResponse
    { _dirInstances = mempty
    }

-- | An array of Instance objects that describe the instances.
dirInstances :: Lens' DescribeInstancesResponse [Instance]
dirInstances = lens _dirInstances (\s a -> s { _dirInstances = a })

-- FromJSON

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = OpsWorks
    type Rs DescribeInstances = DescribeInstancesResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeInstancesResponse
        <$> o .: "Instances"

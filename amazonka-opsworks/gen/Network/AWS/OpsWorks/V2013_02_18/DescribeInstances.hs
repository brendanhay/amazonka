{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a set of instances. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeInstances
    (
    -- * Request
      DescribeInstances
    -- ** Request constructor
    , mkDescribeInstances
    -- ** Request lenses
    , di1StackId
    , di1LayerId
    , di1InstanceIds

    -- * Response
    , DescribeInstancesResponse
    -- ** Response lenses
    , dirsInstances
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeInstances = DescribeInstances
    { _di1StackId :: Maybe Text
    , _di1LayerId :: Maybe Text
    , _di1InstanceIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstances' request.
mkDescribeInstances :: DescribeInstances
mkDescribeInstances = DescribeInstances
    { _di1StackId = Nothing
    , _di1LayerId = Nothing
    , _di1InstanceIds = mempty
    }
{-# INLINE mkDescribeInstances #-}

-- | A stack ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified stack.
di1StackId :: Lens' DescribeInstances (Maybe Text)
di1StackId = lens _di1StackId (\s a -> s { _di1StackId = a })
{-# INLINE di1StackId #-}

-- | A layer ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified layer.
di1LayerId :: Lens' DescribeInstances (Maybe Text)
di1LayerId = lens _di1LayerId (\s a -> s { _di1LayerId = a })
{-# INLINE di1LayerId #-}

-- | An array of instance IDs to be described. If you use this parameter,
-- DescribeInstances returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
di1InstanceIds :: Lens' DescribeInstances [Text]
di1InstanceIds = lens _di1InstanceIds (\s a -> s { _di1InstanceIds = a })
{-# INLINE di1InstanceIds #-}

instance ToPath DescribeInstances

instance ToQuery DescribeInstances

instance ToHeaders DescribeInstances

instance ToJSON DescribeInstances

-- | Contains the response to a DescribeInstances request.
newtype DescribeInstancesResponse = DescribeInstancesResponse
    { _dirsInstances :: [Instance]
    } deriving (Show, Generic)

-- | An array of Instance objects that describe the instances.
dirsInstances :: Lens' DescribeInstancesResponse [Instance]
dirsInstances = lens _dirsInstances (\s a -> s { _dirsInstances = a })
{-# INLINE dirsInstances #-}

instance FromJSON DescribeInstancesResponse

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = OpsWorks
    type Rs DescribeInstances = DescribeInstancesResponse

    request = get
    response _ = jsonResponse

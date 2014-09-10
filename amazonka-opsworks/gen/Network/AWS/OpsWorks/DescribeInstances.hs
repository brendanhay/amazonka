{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
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
module Network.AWS.OpsWorks
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
    -- ** Response constructor
    , mkDescribeInstancesResponse
    -- ** Response lenses
    , dirInstances
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeInstances = DescribeInstances
    { _di1StackId :: !(Maybe Text)
    , _di1LayerId :: !(Maybe Text)
    , _di1InstanceIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerId ::@ @Maybe Text@
--
-- * @InstanceIds ::@ @[Text]@
--
mkDescribeInstances :: DescribeInstances
mkDescribeInstances = DescribeInstances
    { _di1StackId = Nothing
    , _di1LayerId = Nothing
    , _di1InstanceIds = mempty
    }

-- | A stack ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified stack.
di1StackId :: Lens' DescribeInstances (Maybe Text)
di1StackId = lens _di1StackId (\s a -> s { _di1StackId = a })

-- | A layer ID. If you use this parameter, DescribeInstances returns
-- descriptions of the instances associated with the specified layer.
di1LayerId :: Lens' DescribeInstances (Maybe Text)
di1LayerId = lens _di1LayerId (\s a -> s { _di1LayerId = a })

-- | An array of instance IDs to be described. If you use this parameter,
-- DescribeInstances returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
di1InstanceIds :: Lens' DescribeInstances [Text]
di1InstanceIds = lens _di1InstanceIds (\s a -> s { _di1InstanceIds = a })

instance ToPath DescribeInstances

instance ToQuery DescribeInstances

instance ToHeaders DescribeInstances

instance ToJSON DescribeInstances

-- | Contains the response to a DescribeInstances request.
newtype DescribeInstancesResponse = DescribeInstancesResponse
    { _dirInstances :: [Instance]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Instances ::@ @[Instance]@
--
mkDescribeInstancesResponse :: DescribeInstancesResponse
mkDescribeInstancesResponse = DescribeInstancesResponse
    { _dirInstances = mempty
    }

-- | An array of Instance objects that describe the instances.
dirInstances :: Lens' DescribeInstancesResponse [Instance]
dirInstances = lens _dirInstances (\s a -> s { _dirInstances = a })

instance FromJSON DescribeInstancesResponse

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = OpsWorks
    type Rs DescribeInstances = DescribeInstancesResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeRaidArrays
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describe an instance's RAID arrays. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeRaidArrays
    (
    -- * Request
      DescribeRaidArrays
    -- ** Request constructor
    , describeRaidArrays
    -- ** Request lenses
    , draInstanceId
    , draStackId
    , draRaidArrayIds

    -- * Response
    , DescribeRaidArraysResponse
    -- ** Response constructor
    , describeRaidArraysResponse
    -- ** Response lenses
    , drarRaidArrays
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeRaidArrays = DescribeRaidArrays
    { _draInstanceId :: Maybe Text
    , _draStackId :: Maybe Text
    , _draRaidArrayIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRaidArrays' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @RaidArrayIds ::@ @[Text]@
--
describeRaidArrays :: DescribeRaidArrays
describeRaidArrays = DescribeRaidArrays
    { _draInstanceId = Nothing
    , _draStackId = Nothing
    , _draRaidArrayIds = mempty
    }

-- | The instance ID. If you use this parameter, DescribeRaidArrays returns
-- descriptions of the RAID arrays associated with the specified instance.
draInstanceId :: Lens' DescribeRaidArrays (Maybe Text)
draInstanceId = lens _draInstanceId (\s a -> s { _draInstanceId = a })

-- | The stack ID.
draStackId :: Lens' DescribeRaidArrays (Maybe Text)
draStackId = lens _draStackId (\s a -> s { _draStackId = a })

-- | An array of RAID array IDs. If you use this parameter, DescribeRaidArrays
-- returns descriptions of the specified arrays. Otherwise, it returns a
-- description of every array.
draRaidArrayIds :: Lens' DescribeRaidArrays [Text]
draRaidArrayIds = lens _draRaidArrayIds (\s a -> s { _draRaidArrayIds = a })

instance ToPath DescribeRaidArrays

instance ToQuery DescribeRaidArrays

instance ToHeaders DescribeRaidArrays

instance ToJSON DescribeRaidArrays

-- | Contains the response to a DescribeRaidArrays request.
newtype DescribeRaidArraysResponse = DescribeRaidArraysResponse
    { _drarRaidArrays :: [RaidArray]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRaidArraysResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RaidArrays ::@ @[RaidArray]@
--
describeRaidArraysResponse :: DescribeRaidArraysResponse
describeRaidArraysResponse = DescribeRaidArraysResponse
    { _drarRaidArrays = mempty
    }

-- | A RaidArrays object that describes the specified RAID arrays.
drarRaidArrays :: Lens' DescribeRaidArraysResponse [RaidArray]
drarRaidArrays = lens _drarRaidArrays (\s a -> s { _drarRaidArrays = a })

instance FromJSON DescribeRaidArraysResponse

instance AWSRequest DescribeRaidArrays where
    type Sv DescribeRaidArrays = OpsWorks
    type Rs DescribeRaidArrays = DescribeRaidArraysResponse

    request = get
    response _ = jsonResponse

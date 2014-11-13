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

-- Module      : Network.AWS.OpsWorks.DescribeRaidArrays
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describe an instance's RAID arrays. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeRaidArrays
    (
    -- * Request
      DescribeRaidArrays
    -- ** Request constructor
    , describeRaidArrays
    -- ** Request lenses
    , draInstanceId
    , draRaidArrayIds

    -- * Response
    , DescribeRaidArraysResponse
    -- ** Response constructor
    , describeRaidArraysResponse
    -- ** Response lenses
    , drarRaidArrays
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeRaidArrays = DescribeRaidArrays
    { _draInstanceId   :: Maybe Text
    , _draRaidArrayIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeRaidArrays' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'draInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'draRaidArrayIds' @::@ ['Text']
--
describeRaidArrays :: DescribeRaidArrays
describeRaidArrays = DescribeRaidArrays
    { _draInstanceId   = Nothing
    , _draRaidArrayIds = mempty
    }

-- | The instance ID. If you use this parameter, DescribeRaidArrays returns
-- descriptions of the RAID arrays associated with the specified instance.
draInstanceId :: Lens' DescribeRaidArrays (Maybe Text)
draInstanceId = lens _draInstanceId (\s a -> s { _draInstanceId = a })

-- | An array of RAID array IDs. If you use this parameter, DescribeRaidArrays
-- returns descriptions of the specified arrays. Otherwise, it returns a
-- description of every array.
draRaidArrayIds :: Lens' DescribeRaidArrays [Text]
draRaidArrayIds = lens _draRaidArrayIds (\s a -> s { _draRaidArrayIds = a })

instance ToPath DescribeRaidArrays where
    toPath = const "/"

instance ToQuery DescribeRaidArrays where
    toQuery = const mempty

instance ToHeaders DescribeRaidArrays

instance ToBody DescribeRaidArrays where
    toBody = toBody . encode . _draInstanceId

newtype DescribeRaidArraysResponse = DescribeRaidArraysResponse
    { _drarRaidArrays :: [RaidArray]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeRaidArraysResponse where
    type Item DescribeRaidArraysResponse = RaidArray

    fromList = DescribeRaidArraysResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _drarRaidArrays

-- | 'DescribeRaidArraysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drarRaidArrays' @::@ ['RaidArray']
--
describeRaidArraysResponse :: DescribeRaidArraysResponse
describeRaidArraysResponse = DescribeRaidArraysResponse
    { _drarRaidArrays = mempty
    }

-- | A RaidArrays object that describes the specified RAID arrays.
drarRaidArrays :: Lens' DescribeRaidArraysResponse [RaidArray]
drarRaidArrays = lens _drarRaidArrays (\s a -> s { _drarRaidArrays = a })

-- FromJSON

instance AWSRequest DescribeRaidArrays where
    type Sv DescribeRaidArrays = OpsWorks
    type Rs DescribeRaidArrays = DescribeRaidArraysResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeRaidArraysResponse
        <$> o .: "RaidArrays"

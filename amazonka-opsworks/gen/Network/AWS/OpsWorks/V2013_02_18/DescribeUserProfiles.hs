{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeUserProfiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describe specified users. Required Permissions: To use this action, an IAM
-- user must have an attached policy that explicitly grants permissions. For
-- more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeUserProfiles
    (
    -- * Request
      DescribeUserProfiles
    -- ** Request constructor
    , describeUserProfiles
    -- ** Request lenses
    , dupsIamUserArns

    -- * Response
    , DescribeUserProfilesResponse
    -- ** Response lenses
    , duptUserProfiles
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeUserProfiles' request.
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles = DescribeUserProfiles
    { _dupsIamUserArns = mempty
    }

data DescribeUserProfiles = DescribeUserProfiles
    { _dupsIamUserArns :: [Text]
      -- ^ An array of IAM user ARNs that identify the users to be
      -- described.
    } deriving (Show, Generic)

-- | An array of IAM user ARNs that identify the users to be described.
dupsIamUserArns
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeUserProfiles
    -> f DescribeUserProfiles
dupsIamUserArns f x =
    (\y -> x { _dupsIamUserArns = y })
       <$> f (_dupsIamUserArns x)
{-# INLINE dupsIamUserArns #-}

instance ToPath DescribeUserProfiles

instance ToQuery DescribeUserProfiles

instance ToHeaders DescribeUserProfiles

instance ToJSON DescribeUserProfiles

data DescribeUserProfilesResponse = DescribeUserProfilesResponse
    { _duptUserProfiles :: [UserProfile]
      -- ^ A Users object that describes the specified users.
    } deriving (Show, Generic)

-- | A Users object that describes the specified users.
duptUserProfiles
    :: Functor f
    => ([UserProfile]
    -> f ([UserProfile]))
    -> DescribeUserProfilesResponse
    -> f DescribeUserProfilesResponse
duptUserProfiles f x =
    (\y -> x { _duptUserProfiles = y })
       <$> f (_duptUserProfiles x)
{-# INLINE duptUserProfiles #-}

instance FromJSON DescribeUserProfilesResponse

instance AWSRequest DescribeUserProfiles where
    type Sv DescribeUserProfiles = OpsWorks
    type Rs DescribeUserProfiles = DescribeUserProfilesResponse

    request = get
    response _ = jsonResponse

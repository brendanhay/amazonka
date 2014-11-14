{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes a user's SSH information. Required Permissions: To use this
-- action, an IAM user must have self-management enabled or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeMyUserProfile
    (
    -- * Request
      DescribeMyUserProfile
    -- ** Request constructor
    , describeMyUserProfile

    -- * Response
    , DescribeMyUserProfileResponse
    -- ** Response constructor
    , describeMyUserProfileResponse
    -- ** Response lenses
    , dmuprUserProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeMyUserProfile = DescribeMyUserProfile
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeMyUserProfile' constructor.
describeMyUserProfile :: DescribeMyUserProfile
describeMyUserProfile = DescribeMyUserProfile

instance ToPath DescribeMyUserProfile where
    toPath = const "/"

instance ToQuery DescribeMyUserProfile where
    toQuery = const mempty

instance ToHeaders DescribeMyUserProfile

instance ToBody DescribeMyUserProfile

newtype DescribeMyUserProfileResponse = DescribeMyUserProfileResponse
    { _dmuprUserProfile :: Maybe SelfUserProfile
    } deriving (Eq, Show, Generic)

-- | 'DescribeMyUserProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmuprUserProfile' @::@ 'Maybe' 'SelfUserProfile'
--
describeMyUserProfileResponse :: DescribeMyUserProfileResponse
describeMyUserProfileResponse = DescribeMyUserProfileResponse
    { _dmuprUserProfile = Nothing
    }

-- | A UserProfile object that describes the user's SSH information.
dmuprUserProfile :: Lens' DescribeMyUserProfileResponse (Maybe SelfUserProfile)
dmuprUserProfile = lens _dmuprUserProfile (\s a -> s { _dmuprUserProfile = a })

instance AWSRequest DescribeMyUserProfile where
    type Sv DescribeMyUserProfile = OpsWorks
    type Rs DescribeMyUserProfile = DescribeMyUserProfileResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeMyUserProfileResponse
        <$> o .: "UserProfile"

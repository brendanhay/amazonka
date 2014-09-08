{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeMyUserProfile
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
module Network.AWS.OpsWorks.V2013_02_18.DescribeMyUserProfile
    (
    -- * Request
      DescribeMyUserProfile
    -- ** Request constructor
    , mkDescribeMyUserProfile
    -- * Response
    , DescribeMyUserProfileResponse
    -- ** Response lenses
    , dmuprUserProfile
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeMyUserProfile = DescribeMyUserProfile
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMyUserProfile' request.
mkDescribeMyUserProfile :: DescribeMyUserProfile
mkDescribeMyUserProfile = DescribeMyUserProfile

instance ToPath DescribeMyUserProfile

instance ToQuery DescribeMyUserProfile

instance ToHeaders DescribeMyUserProfile

instance ToJSON DescribeMyUserProfile

-- | Contains the response to a DescribeMyUserProfile request.
newtype DescribeMyUserProfileResponse = DescribeMyUserProfileResponse
    { _dmuprUserProfile :: Maybe SelfUserProfile
    } deriving (Show, Generic)

-- | A UserProfile object that describes the user's SSH information.
dmuprUserProfile :: Lens' DescribeMyUserProfileResponse (Maybe SelfUserProfile)
dmuprUserProfile =
    lens _dmuprUserProfile (\s a -> s { _dmuprUserProfile = a })

instance FromJSON DescribeMyUserProfileResponse

instance AWSRequest DescribeMyUserProfile where
    type Sv DescribeMyUserProfile = OpsWorks
    type Rs DescribeMyUserProfile = DescribeMyUserProfileResponse

    request = get
    response _ = jsonResponse

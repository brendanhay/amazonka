{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeUserProfiles where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeUserProfiles' request.
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles = DescribeUserProfiles
    { _duprIamUserArns = mempty
    }

data DescribeUserProfiles = DescribeUserProfiles
    { _duprIamUserArns :: [Text]
      -- ^ An array of IAM user ARNs that identify the users to be
      -- described.
    } deriving (Show, Generic)

makeLenses ''DescribeUserProfiles

instance ToPath DescribeUserProfiles

instance ToQuery DescribeUserProfiles

instance ToHeaders DescribeUserProfiles

instance ToJSON DescribeUserProfiles

data DescribeUserProfilesResponse = DescribeUserProfilesResponse
    { _dupsUserProfiles :: [UserProfile]
      -- ^ A Users object that describes the specified users.
    } deriving (Show, Generic)

makeLenses ''DescribeUserProfilesResponse

instance FromJSON DescribeUserProfilesResponse

instance AWSRequest DescribeUserProfiles where
    type Sv DescribeUserProfiles = OpsWorks
    type Rs DescribeUserProfiles = DescribeUserProfilesResponse

    request = get
    response _ = undefined

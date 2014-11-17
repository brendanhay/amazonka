{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
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
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeUserProfiles.html>
module Network.AWS.OpsWorks.DescribeUserProfiles
    (
    -- * Request
      DescribeUserProfiles
    -- ** Request constructor
    , describeUserProfiles
    -- ** Request lenses
    , dupIamUserArns

    -- * Response
    , DescribeUserProfilesResponse
    -- ** Response constructor
    , describeUserProfilesResponse
    -- ** Response lenses
    , duprUserProfiles
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DescribeUserProfiles = DescribeUserProfiles
    { _dupIamUserArns :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeUserProfiles where
    type Item DescribeUserProfiles = Text

    fromList = DescribeUserProfiles . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dupIamUserArns

-- | 'DescribeUserProfiles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupIamUserArns' @::@ ['Text']
--
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles = DescribeUserProfiles
    { _dupIamUserArns = mempty
    }

-- | An array of IAM user ARNs that identify the users to be described.
dupIamUserArns :: Lens' DescribeUserProfiles [Text]
dupIamUserArns = lens _dupIamUserArns (\s a -> s { _dupIamUserArns = a })

newtype DescribeUserProfilesResponse = DescribeUserProfilesResponse
    { _duprUserProfiles :: [UserProfile]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeUserProfilesResponse where
    type Item DescribeUserProfilesResponse = UserProfile

    fromList = DescribeUserProfilesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _duprUserProfiles

-- | 'DescribeUserProfilesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'duprUserProfiles' @::@ ['UserProfile']
--
describeUserProfilesResponse :: DescribeUserProfilesResponse
describeUserProfilesResponse = DescribeUserProfilesResponse
    { _duprUserProfiles = mempty
    }

-- | A Users object that describes the specified users.
duprUserProfiles :: Lens' DescribeUserProfilesResponse [UserProfile]
duprUserProfiles = lens _duprUserProfiles (\s a -> s { _duprUserProfiles = a })

instance AWSRequest DescribeUserProfiles where
    type Sv DescribeUserProfiles = OpsWorks
    type Rs DescribeUserProfiles = DescribeUserProfilesResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeUserProfilesResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeUserProfiles where
    toPath = const "/"

instance ToHeaders DescribeUserProfiles

instance ToQuery DescribeUserProfiles where
    toQuery = const mempty

instance ToJSON DescribeUserProfiles where
    toJSON = genericToJSON jsonOptions

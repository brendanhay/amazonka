{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describe specified users.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeUserProfiles.html>
module Network.AWS.OpsWorks.DescribeUserProfiles
    (
    -- * Request
      DescribeUserProfiles
    -- ** Request constructor
    , describeUserProfiles
    -- ** Request lenses
    , dupIAMUserARNs

    -- * Response
    , DescribeUserProfilesResponse
    -- ** Response constructor
    , describeUserProfilesResponse
    -- ** Response lenses
    , duprUserProfiles
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'describeUserProfiles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupIAMUserARNs'
newtype DescribeUserProfiles = DescribeUserProfiles'{_dupIAMUserARNs :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeUserProfiles' smart constructor.
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles = DescribeUserProfiles'{_dupIAMUserARNs = Nothing};

-- | An array of IAM user ARNs that identify the users to be described.
dupIAMUserARNs :: Lens' DescribeUserProfiles (Maybe [Text])
dupIAMUserARNs = lens _dupIAMUserARNs (\ s a -> s{_dupIAMUserARNs = a});

instance AWSRequest DescribeUserProfiles where
        type Sv DescribeUserProfiles = OpsWorks
        type Rs DescribeUserProfiles =
             DescribeUserProfilesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserProfilesResponse' <$>
                   x .?> "UserProfiles" .!@ mempty)

instance ToHeaders DescribeUserProfiles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeUserProfiles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserProfiles where
        toJSON DescribeUserProfiles'{..}
          = object ["IamUserArns" .= _dupIAMUserARNs]

instance ToPath DescribeUserProfiles where
        toPath = const "/"

instance ToQuery DescribeUserProfiles where
        toQuery = const mempty

-- | /See:/ 'describeUserProfilesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'duprUserProfiles'
newtype DescribeUserProfilesResponse = DescribeUserProfilesResponse'{_duprUserProfiles :: Maybe [UserProfile]} deriving (Eq, Read, Show)

-- | 'DescribeUserProfilesResponse' smart constructor.
describeUserProfilesResponse :: DescribeUserProfilesResponse
describeUserProfilesResponse = DescribeUserProfilesResponse'{_duprUserProfiles = Nothing};

-- | A @Users@ object that describes the specified users.
duprUserProfiles :: Lens' DescribeUserProfilesResponse (Maybe [UserProfile])
duprUserProfiles = lens _duprUserProfiles (\ s a -> s{_duprUserProfiles = a});

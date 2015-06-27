{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
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

-- | Describes a user\'s SSH information.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeMyUserProfile.html>
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
    , dmuprStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMyUserProfile' smart constructor.
data DescribeMyUserProfile =
    DescribeMyUserProfile'
    deriving (Eq,Read,Show)

-- | 'DescribeMyUserProfile' smart constructor.
describeMyUserProfile :: DescribeMyUserProfile
describeMyUserProfile = DescribeMyUserProfile'

instance AWSRequest DescribeMyUserProfile where
        type Sv DescribeMyUserProfile = OpsWorks
        type Rs DescribeMyUserProfile =
             DescribeMyUserProfileResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMyUserProfileResponse' <$>
                   (x .?> "UserProfile") <*> (pure (fromEnum s)))

instance ToHeaders DescribeMyUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeMyUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMyUserProfile where
        toJSON = const (Object mempty)

instance ToPath DescribeMyUserProfile where
        toPath = const "/"

instance ToQuery DescribeMyUserProfile where
        toQuery = const mempty

-- | Contains the response to a @DescribeMyUserProfile@ request.
--
-- /See:/ 'describeMyUserProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmuprUserProfile'
--
-- * 'dmuprStatus'
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
    { _dmuprUserProfile :: Maybe SelfUserProfile
    , _dmuprStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeMyUserProfileResponse' smart constructor.
describeMyUserProfileResponse :: Int -> DescribeMyUserProfileResponse
describeMyUserProfileResponse pStatus =
    DescribeMyUserProfileResponse'
    { _dmuprUserProfile = Nothing
    , _dmuprStatus = pStatus
    }

-- | A @UserProfile@ object that describes the user\'s SSH information.
dmuprUserProfile :: Lens' DescribeMyUserProfileResponse (Maybe SelfUserProfile)
dmuprUserProfile = lens _dmuprUserProfile (\ s a -> s{_dmuprUserProfile = a});

-- | FIXME: Undocumented member.
dmuprStatus :: Lens' DescribeMyUserProfileResponse Int
dmuprStatus = lens _dmuprStatus (\ s a -> s{_dmuprStatus = a});

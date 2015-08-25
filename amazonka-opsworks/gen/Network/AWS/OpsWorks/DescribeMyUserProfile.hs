{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user\'s SSH information.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeMyUserProfile.html AWS API Reference> for DescribeMyUserProfile.
module Network.AWS.OpsWorks.DescribeMyUserProfile
    (
    -- * Creating a Request
      describeMyUserProfile
    , DescribeMyUserProfile

    -- * Destructuring the Response
    , describeMyUserProfileResponse
    , DescribeMyUserProfileResponse
    -- * Response Lenses
    , dmuprsUserProfile
    , dmuprsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMyUserProfile' smart constructor.
data DescribeMyUserProfile =
    DescribeMyUserProfile'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMyUserProfile' with the minimum fields required to make a request.
--
describeMyUserProfile
    :: DescribeMyUserProfile
describeMyUserProfile = DescribeMyUserProfile'

instance AWSRequest DescribeMyUserProfile where
        type Rs DescribeMyUserProfile =
             DescribeMyUserProfileResponse
        request = postJSON opsWorks
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

-- | Contains the response to a 'DescribeMyUserProfile' request.
--
-- /See:/ 'describeMyUserProfileResponse' smart constructor.
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
    { _dmuprsUserProfile :: !(Maybe SelfUserProfile)
    , _dmuprsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMyUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmuprsUserProfile'
--
-- * 'dmuprsStatus'
describeMyUserProfileResponse
    :: Int -- ^ 'dmuprsStatus'
    -> DescribeMyUserProfileResponse
describeMyUserProfileResponse pStatus_ =
    DescribeMyUserProfileResponse'
    { _dmuprsUserProfile = Nothing
    , _dmuprsStatus = pStatus_
    }

-- | A 'UserProfile' object that describes the user\'s SSH information.
dmuprsUserProfile :: Lens' DescribeMyUserProfileResponse (Maybe SelfUserProfile)
dmuprsUserProfile = lens _dmuprsUserProfile (\ s a -> s{_dmuprsUserProfile = a});

-- | The response status code.
dmuprsStatus :: Lens' DescribeMyUserProfileResponse Int
dmuprsStatus = lens _dmuprsStatus (\ s a -> s{_dmuprsStatus = a});

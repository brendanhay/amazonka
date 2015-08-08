{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describe specified users.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeUserProfiles.html AWS API Reference> for DescribeUserProfiles.
module Network.AWS.OpsWorks.DescribeUserProfiles
    (
    -- * Creating a Request
      DescribeUserProfiles
    , describeUserProfiles
    -- * Request Lenses
    , dupIAMUserARNs

    -- * Destructuring the Response
    , DescribeUserProfilesResponse
    , describeUserProfilesResponse
    -- * Response Lenses
    , duprsUserProfiles
    , duprsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeUserProfiles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupIAMUserARNs'
newtype DescribeUserProfiles = DescribeUserProfiles'
    { _dupIAMUserARNs :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeUserProfiles' smart constructor.
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles =
    DescribeUserProfiles'
    { _dupIAMUserARNs = Nothing
    }

-- | An array of IAM user ARNs that identify the users to be described.
dupIAMUserARNs :: Lens' DescribeUserProfiles [Text]
dupIAMUserARNs = lens _dupIAMUserARNs (\ s a -> s{_dupIAMUserARNs = a}) . _Default . _Coerce;

instance AWSRequest DescribeUserProfiles where
        type Sv DescribeUserProfiles = OpsWorks
        type Rs DescribeUserProfiles =
             DescribeUserProfilesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserProfilesResponse' <$>
                   (x .?> "UserProfiles" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | Contains the response to a @DescribeUserProfiles@ request.
--
-- /See:/ 'describeUserProfilesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'duprsUserProfiles'
--
-- * 'duprsStatus'
data DescribeUserProfilesResponse = DescribeUserProfilesResponse'
    { _duprsUserProfiles :: !(Maybe [UserProfile])
    , _duprsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeUserProfilesResponse' smart constructor.
describeUserProfilesResponse :: Int -> DescribeUserProfilesResponse
describeUserProfilesResponse pStatus_ =
    DescribeUserProfilesResponse'
    { _duprsUserProfiles = Nothing
    , _duprsStatus = pStatus_
    }

-- | A @Users@ object that describes the specified users.
duprsUserProfiles :: Lens' DescribeUserProfilesResponse [UserProfile]
duprsUserProfiles = lens _duprsUserProfiles (\ s a -> s{_duprsUserProfiles = a}) . _Default . _Coerce;

-- | Undocumented member.
duprsStatus :: Lens' DescribeUserProfilesResponse Int
duprsStatus = lens _duprsStatus (\ s a -> s{_duprsStatus = a});

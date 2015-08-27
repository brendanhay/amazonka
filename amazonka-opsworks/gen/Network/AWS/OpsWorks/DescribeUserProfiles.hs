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
-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
      describeUserProfiles
    , DescribeUserProfiles
    -- * Request Lenses
    , dupIAMUserARNs

    -- * Destructuring the Response
    , describeUserProfilesResponse
    , DescribeUserProfilesResponse
    -- * Response Lenses
    , duprsUserProfiles
    , duprsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeUserProfiles' smart constructor.
newtype DescribeUserProfiles = DescribeUserProfiles'
    { _dupIAMUserARNs :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeUserProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupIAMUserARNs'
describeUserProfiles
    :: DescribeUserProfiles
describeUserProfiles =
    DescribeUserProfiles'
    { _dupIAMUserARNs = Nothing
    }

-- | An array of IAM user ARNs that identify the users to be described.
dupIAMUserARNs :: Lens' DescribeUserProfiles [Text]
dupIAMUserARNs = lens _dupIAMUserARNs (\ s a -> s{_dupIAMUserARNs = a}) . _Default . _Coerce;

instance AWSRequest DescribeUserProfiles where
        type Rs DescribeUserProfiles =
             DescribeUserProfilesResponse
        request = postJSON opsWorks
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
          = object
              (catMaybes [("IamUserArns" .=) <$> _dupIAMUserARNs])

instance ToPath DescribeUserProfiles where
        toPath = const "/"

instance ToQuery DescribeUserProfiles where
        toQuery = const mempty

-- | Contains the response to a 'DescribeUserProfiles' request.
--
-- /See:/ 'describeUserProfilesResponse' smart constructor.
data DescribeUserProfilesResponse = DescribeUserProfilesResponse'
    { _duprsUserProfiles :: !(Maybe [UserProfile])
    , _duprsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeUserProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duprsUserProfiles'
--
-- * 'duprsStatus'
describeUserProfilesResponse
    :: Int -- ^ 'duprsStatus'
    -> DescribeUserProfilesResponse
describeUserProfilesResponse pStatus_ =
    DescribeUserProfilesResponse'
    { _duprsUserProfiles = Nothing
    , _duprsStatus = pStatus_
    }

-- | A 'Users' object that describes the specified users.
duprsUserProfiles :: Lens' DescribeUserProfilesResponse [UserProfile]
duprsUserProfiles = lens _duprsUserProfiles (\ s a -> s{_duprsUserProfiles = a}) . _Default . _Coerce;

-- | The response status code.
duprsStatus :: Lens' DescribeUserProfilesResponse Int
duprsStatus = lens _duprsStatus (\ s a -> s{_duprsStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the
-- specified user.
--
-- A user can also have managed policies attached to it. To retrieve a
-- managed policy document that is attached to a user, use GetPolicy to
-- determine the policy\'s default version, then use GetPolicyVersion to
-- retrieve the policy document.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserPolicy.html>
module Network.AWS.IAM.GetUserPolicy
    (
    -- * Request
      GetUserPolicy
    -- ** Request constructor
    , getUserPolicy
    -- ** Request lenses
    , guprqUserName
    , guprqPolicyName

    -- * Response
    , GetUserPolicyResponse
    -- ** Response constructor
    , getUserPolicyResponse
    -- ** Response lenses
    , guprsStatus
    , guprsUserName
    , guprsPolicyName
    , guprsPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guprqUserName'
--
-- * 'guprqPolicyName'
data GetUserPolicy = GetUserPolicy'
    { _guprqUserName   :: !Text
    , _guprqPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUserPolicy' smart constructor.
getUserPolicy :: Text -> Text -> GetUserPolicy
getUserPolicy pUserName pPolicyName =
    GetUserPolicy'
    { _guprqUserName = pUserName
    , _guprqPolicyName = pPolicyName
    }

-- | The name of the user who the policy is associated with.
guprqUserName :: Lens' GetUserPolicy Text
guprqUserName = lens _guprqUserName (\ s a -> s{_guprqUserName = a});

-- | The name of the policy document to get.
guprqPolicyName :: Lens' GetUserPolicy Text
guprqPolicyName = lens _guprqPolicyName (\ s a -> s{_guprqPolicyName = a});

instance AWSRequest GetUserPolicy where
        type Sv GetUserPolicy = IAM
        type Rs GetUserPolicy = GetUserPolicyResponse
        request = post
        response
          = receiveXMLWrapper "GetUserPolicyResult"
              (\ s h x ->
                 GetUserPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "UserName") <*>
                     (x .@ "PolicyName")
                     <*> (x .@ "PolicyDocument"))

instance ToHeaders GetUserPolicy where
        toHeaders = const mempty

instance ToPath GetUserPolicy where
        toPath = const "/"

instance ToQuery GetUserPolicy where
        toQuery GetUserPolicy'{..}
          = mconcat
              ["Action" =: ("GetUserPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _guprqUserName,
               "PolicyName" =: _guprqPolicyName]

-- | Contains the response to a successful GetUserPolicy request.
--
-- /See:/ 'getUserPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guprsStatus'
--
-- * 'guprsUserName'
--
-- * 'guprsPolicyName'
--
-- * 'guprsPolicyDocument'
data GetUserPolicyResponse = GetUserPolicyResponse'
    { _guprsStatus         :: !Int
    , _guprsUserName       :: !Text
    , _guprsPolicyName     :: !Text
    , _guprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUserPolicyResponse' smart constructor.
getUserPolicyResponse :: Int -> Text -> Text -> Text -> GetUserPolicyResponse
getUserPolicyResponse pStatus pUserName pPolicyName pPolicyDocument =
    GetUserPolicyResponse'
    { _guprsStatus = pStatus
    , _guprsUserName = pUserName
    , _guprsPolicyName = pPolicyName
    , _guprsPolicyDocument = pPolicyDocument
    }

-- | FIXME: Undocumented member.
guprsStatus :: Lens' GetUserPolicyResponse Int
guprsStatus = lens _guprsStatus (\ s a -> s{_guprsStatus = a});

-- | The user the policy is associated with.
guprsUserName :: Lens' GetUserPolicyResponse Text
guprsUserName = lens _guprsUserName (\ s a -> s{_guprsUserName = a});

-- | The name of the policy.
guprsPolicyName :: Lens' GetUserPolicyResponse Text
guprsPolicyName = lens _guprsPolicyName (\ s a -> s{_guprsPolicyName = a});

-- | The policy document.
guprsPolicyDocument :: Lens' GetUserPolicyResponse Text
guprsPolicyDocument = lens _guprsPolicyDocument (\ s a -> s{_guprsPolicyDocument = a});

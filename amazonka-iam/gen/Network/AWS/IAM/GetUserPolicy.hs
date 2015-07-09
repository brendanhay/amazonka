{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , gupUserName
    , gupPolicyName

    -- * Response
    , GetUserPolicyResponse
    -- ** Response constructor
    , getUserPolicyResponse
    -- ** Response lenses
    , guprStatus
    , guprUserName
    , guprPolicyName
    , guprPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gupUserName'
--
-- * 'gupPolicyName'
data GetUserPolicy = GetUserPolicy'
    { _gupUserName   :: !Text
    , _gupPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUserPolicy' smart constructor.
getUserPolicy :: Text -> Text -> GetUserPolicy
getUserPolicy pUserName pPolicyName =
    GetUserPolicy'
    { _gupUserName = pUserName
    , _gupPolicyName = pPolicyName
    }

-- | The name of the user who the policy is associated with.
gupUserName :: Lens' GetUserPolicy Text
gupUserName = lens _gupUserName (\ s a -> s{_gupUserName = a});

-- | The name of the policy document to get.
gupPolicyName :: Lens' GetUserPolicy Text
gupPolicyName = lens _gupPolicyName (\ s a -> s{_gupPolicyName = a});

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
               "UserName" =: _gupUserName,
               "PolicyName" =: _gupPolicyName]

-- | Contains the response to a successful GetUserPolicy request.
--
-- /See:/ 'getUserPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guprStatus'
--
-- * 'guprUserName'
--
-- * 'guprPolicyName'
--
-- * 'guprPolicyDocument'
data GetUserPolicyResponse = GetUserPolicyResponse'
    { _guprStatus         :: !Int
    , _guprUserName       :: !Text
    , _guprPolicyName     :: !Text
    , _guprPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUserPolicyResponse' smart constructor.
getUserPolicyResponse :: Int -> Text -> Text -> Text -> GetUserPolicyResponse
getUserPolicyResponse pStatus pUserName pPolicyName pPolicyDocument =
    GetUserPolicyResponse'
    { _guprStatus = pStatus
    , _guprUserName = pUserName
    , _guprPolicyName = pPolicyName
    , _guprPolicyDocument = pPolicyDocument
    }

-- | FIXME: Undocumented member.
guprStatus :: Lens' GetUserPolicyResponse Int
guprStatus = lens _guprStatus (\ s a -> s{_guprStatus = a});

-- | The user the policy is associated with.
guprUserName :: Lens' GetUserPolicyResponse Text
guprUserName = lens _guprUserName (\ s a -> s{_guprUserName = a});

-- | The name of the policy.
guprPolicyName :: Lens' GetUserPolicyResponse Text
guprPolicyName = lens _guprPolicyName (\ s a -> s{_guprPolicyName = a});

-- | The policy document.
guprPolicyDocument :: Lens' GetUserPolicyResponse Text
guprPolicyDocument = lens _guprPolicyDocument (\ s a -> s{_guprPolicyDocument = a});

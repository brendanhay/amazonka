{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetUserPolicy
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

-- | Retrieves the specified inline policy document that is embedded in the
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
    , guprUserName
    , guprPolicyName
    , guprPolicyDocument
    , guprStatusCode
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gupUserName'
--
-- * 'gupPolicyName'
data GetUserPolicy = GetUserPolicy'{_gupUserName :: Text, _gupPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'GetUserPolicy' smart constructor.
getUserPolicy :: Text -> Text -> GetUserPolicy
getUserPolicy pUserName pPolicyName = GetUserPolicy'{_gupUserName = pUserName, _gupPolicyName = pPolicyName};

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
                   (x .@ "UserName") <*> (x .@ "PolicyName") <*>
                     (x .@ "PolicyDocument")
                     <*> (pure (fromEnum s)))

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
-- * 'guprUserName'
--
-- * 'guprPolicyName'
--
-- * 'guprPolicyDocument'
--
-- * 'guprStatusCode'
data GetUserPolicyResponse = GetUserPolicyResponse'{_guprUserName :: Text, _guprPolicyName :: Text, _guprPolicyDocument :: Text, _guprStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetUserPolicyResponse' smart constructor.
getUserPolicyResponse :: Text -> Text -> Text -> Int -> GetUserPolicyResponse
getUserPolicyResponse pUserName pPolicyName pPolicyDocument pStatusCode = GetUserPolicyResponse'{_guprUserName = pUserName, _guprPolicyName = pPolicyName, _guprPolicyDocument = pPolicyDocument, _guprStatusCode = pStatusCode};

-- | The user the policy is associated with.
guprUserName :: Lens' GetUserPolicyResponse Text
guprUserName = lens _guprUserName (\ s a -> s{_guprUserName = a});

-- | The name of the policy.
guprPolicyName :: Lens' GetUserPolicyResponse Text
guprPolicyName = lens _guprPolicyName (\ s a -> s{_guprPolicyName = a});

-- | The policy document.
guprPolicyDocument :: Lens' GetUserPolicyResponse Text
guprPolicyDocument = lens _guprPolicyDocument (\ s a -> s{_guprPolicyDocument = a});

-- | FIXME: Undocumented member.
guprStatusCode :: Lens' GetUserPolicyResponse Int
guprStatusCode = lens _guprStatusCode (\ s a -> s{_guprStatusCode = a});

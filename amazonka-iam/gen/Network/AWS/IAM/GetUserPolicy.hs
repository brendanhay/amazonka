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
-- Module      : Network.AWS.IAM.GetUserPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the specified IAM user.
--
--
-- An IAM user can also have managed policies attached to it. To retrieve a managed policy document that is attached to a user, use 'GetPolicy' to determine the policy's default version, then use 'GetPolicyVersion' to retrieve the policy document.
--
-- For more information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.GetUserPolicy
    (
    -- * Creating a Request
      getUserPolicy
    , GetUserPolicy
    -- * Request Lenses
    , gupUserName
    , gupPolicyName

    -- * Destructuring the Response
    , getUserPolicyResponse
    , GetUserPolicyResponse
    -- * Response Lenses
    , guprsResponseStatus
    , guprsUserName
    , guprsPolicyName
    , guprsPolicyDocument
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserPolicy' smart constructor.
data GetUserPolicy = GetUserPolicy'
  { _gupUserName   :: !Text
  , _gupPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupUserName' - The name of the user who the policy is associated with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'gupPolicyName' - The name of the policy document to get. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
getUserPolicy
    :: Text -- ^ 'gupUserName'
    -> Text -- ^ 'gupPolicyName'
    -> GetUserPolicy
getUserPolicy pUserName_ pPolicyName_ =
  GetUserPolicy' {_gupUserName = pUserName_, _gupPolicyName = pPolicyName_}


-- | The name of the user who the policy is associated with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
gupUserName :: Lens' GetUserPolicy Text
gupUserName = lens _gupUserName (\ s a -> s{_gupUserName = a})

-- | The name of the policy document to get. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
gupPolicyName :: Lens' GetUserPolicy Text
gupPolicyName = lens _gupPolicyName (\ s a -> s{_gupPolicyName = a})

instance AWSRequest GetUserPolicy where
        type Rs GetUserPolicy = GetUserPolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetUserPolicyResult"
              (\ s h x ->
                 GetUserPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "UserName") <*>
                     (x .@ "PolicyName")
                     <*> (x .@ "PolicyDocument"))

instance Hashable GetUserPolicy where

instance NFData GetUserPolicy where

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

-- | Contains the response to a successful 'GetUserPolicy' request.
--
--
--
-- /See:/ 'getUserPolicyResponse' smart constructor.
data GetUserPolicyResponse = GetUserPolicyResponse'
  { _guprsResponseStatus :: !Int
  , _guprsUserName       :: !Text
  , _guprsPolicyName     :: !Text
  , _guprsPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guprsResponseStatus' - -- | The response status code.
--
-- * 'guprsUserName' - The user the policy is associated with.
--
-- * 'guprsPolicyName' - The name of the policy.
--
-- * 'guprsPolicyDocument' - The policy document.
getUserPolicyResponse
    :: Int -- ^ 'guprsResponseStatus'
    -> Text -- ^ 'guprsUserName'
    -> Text -- ^ 'guprsPolicyName'
    -> Text -- ^ 'guprsPolicyDocument'
    -> GetUserPolicyResponse
getUserPolicyResponse pResponseStatus_ pUserName_ pPolicyName_ pPolicyDocument_ =
  GetUserPolicyResponse'
    { _guprsResponseStatus = pResponseStatus_
    , _guprsUserName = pUserName_
    , _guprsPolicyName = pPolicyName_
    , _guprsPolicyDocument = pPolicyDocument_
    }


-- | -- | The response status code.
guprsResponseStatus :: Lens' GetUserPolicyResponse Int
guprsResponseStatus = lens _guprsResponseStatus (\ s a -> s{_guprsResponseStatus = a})

-- | The user the policy is associated with.
guprsUserName :: Lens' GetUserPolicyResponse Text
guprsUserName = lens _guprsUserName (\ s a -> s{_guprsUserName = a})

-- | The name of the policy.
guprsPolicyName :: Lens' GetUserPolicyResponse Text
guprsPolicyName = lens _guprsPolicyName (\ s a -> s{_guprsPolicyName = a})

-- | The policy document.
guprsPolicyDocument :: Lens' GetUserPolicyResponse Text
guprsPolicyDocument = lens _guprsPolicyDocument (\ s a -> s{_guprsPolicyDocument = a})

instance NFData GetUserPolicyResponse where

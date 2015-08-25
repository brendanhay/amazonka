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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserPolicy.html AWS API Reference> for GetUserPolicy.
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
    , guprsStatus
    , guprsUserName
    , guprsPolicyName
    , guprsPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getUserPolicy' smart constructor.
data GetUserPolicy = GetUserPolicy'
    { _gupUserName   :: !Text
    , _gupPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupUserName'
--
-- * 'gupPolicyName'
getUserPolicy
    :: Text -- ^ 'gupUserName'
    -> Text -- ^ 'gupPolicyName'
    -> GetUserPolicy
getUserPolicy pUserName_ pPolicyName_ =
    GetUserPolicy'
    { _gupUserName = pUserName_
    , _gupPolicyName = pPolicyName_
    }

-- | The name of the user who the policy is associated with.
gupUserName :: Lens' GetUserPolicy Text
gupUserName = lens _gupUserName (\ s a -> s{_gupUserName = a});

-- | The name of the policy document to get.
gupPolicyName :: Lens' GetUserPolicy Text
gupPolicyName = lens _gupPolicyName (\ s a -> s{_gupPolicyName = a});

instance AWSRequest GetUserPolicy where
        type Rs GetUserPolicy = GetUserPolicyResponse
        request = postQuery iAM
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
data GetUserPolicyResponse = GetUserPolicyResponse'
    { _guprsStatus         :: !Int
    , _guprsUserName       :: !Text
    , _guprsPolicyName     :: !Text
    , _guprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetUserPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guprsStatus'
--
-- * 'guprsUserName'
--
-- * 'guprsPolicyName'
--
-- * 'guprsPolicyDocument'
getUserPolicyResponse
    :: Int -- ^ 'guprsStatus'
    -> Text -- ^ 'guprsUserName'
    -> Text -- ^ 'guprsPolicyName'
    -> Text -- ^ 'guprsPolicyDocument'
    -> GetUserPolicyResponse
getUserPolicyResponse pStatus_ pUserName_ pPolicyName_ pPolicyDocument_ =
    GetUserPolicyResponse'
    { _guprsStatus = pStatus_
    , _guprsUserName = pUserName_
    , _guprsPolicyName = pPolicyName_
    , _guprsPolicyDocument = pPolicyDocument_
    }

-- | The response status code.
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

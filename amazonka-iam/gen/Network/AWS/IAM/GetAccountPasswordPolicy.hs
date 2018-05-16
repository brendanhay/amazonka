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
-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password policy for the AWS account. For more information about using a password policy, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy> .
--
--
module Network.AWS.IAM.GetAccountPasswordPolicy
    (
    -- * Creating a Request
      getAccountPasswordPolicy
    , GetAccountPasswordPolicy

    -- * Destructuring the Response
    , getAccountPasswordPolicyResponse
    , GetAccountPasswordPolicyResponse
    -- * Response Lenses
    , gapprsResponseStatus
    , gapprsPasswordPolicy
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy =
  GetAccountPasswordPolicy'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountPasswordPolicy' with the minimum fields required to make a request.
--
getAccountPasswordPolicy
    :: GetAccountPasswordPolicy
getAccountPasswordPolicy = GetAccountPasswordPolicy'


instance AWSRequest GetAccountPasswordPolicy where
        type Rs GetAccountPasswordPolicy =
             GetAccountPasswordPolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetAccountPasswordPolicyResult"
              (\ s h x ->
                 GetAccountPasswordPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "PasswordPolicy"))

instance Hashable GetAccountPasswordPolicy where

instance NFData GetAccountPasswordPolicy where

instance ToHeaders GetAccountPasswordPolicy where
        toHeaders = const mempty

instance ToPath GetAccountPasswordPolicy where
        toPath = const "/"

instance ToQuery GetAccountPasswordPolicy where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("GetAccountPasswordPolicy" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful 'GetAccountPasswordPolicy' request.
--
--
--
-- /See:/ 'getAccountPasswordPolicyResponse' smart constructor.
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
  { _gapprsResponseStatus :: !Int
  , _gapprsPasswordPolicy :: !PasswordPolicy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountPasswordPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapprsResponseStatus' - -- | The response status code.
--
-- * 'gapprsPasswordPolicy' - A structure that contains details about the account's password policy.
getAccountPasswordPolicyResponse
    :: Int -- ^ 'gapprsResponseStatus'
    -> PasswordPolicy -- ^ 'gapprsPasswordPolicy'
    -> GetAccountPasswordPolicyResponse
getAccountPasswordPolicyResponse pResponseStatus_ pPasswordPolicy_ =
  GetAccountPasswordPolicyResponse'
    { _gapprsResponseStatus = pResponseStatus_
    , _gapprsPasswordPolicy = pPasswordPolicy_
    }


-- | -- | The response status code.
gapprsResponseStatus :: Lens' GetAccountPasswordPolicyResponse Int
gapprsResponseStatus = lens _gapprsResponseStatus (\ s a -> s{_gapprsResponseStatus = a})

-- | A structure that contains details about the account's password policy.
gapprsPasswordPolicy :: Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprsPasswordPolicy = lens _gapprsPasswordPolicy (\ s a -> s{_gapprsPasswordPolicy = a})

instance NFData GetAccountPasswordPolicyResponse
         where

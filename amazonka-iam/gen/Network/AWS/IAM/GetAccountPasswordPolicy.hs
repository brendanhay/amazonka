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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy>.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountPasswordPolicy.html AWS API Reference> for GetAccountPasswordPolicy.
module Network.AWS.IAM.GetAccountPasswordPolicy
    (
    -- * Creating a Request
      getAccountPasswordPolicy
    , GetAccountPasswordPolicy

    -- * Destructuring the Response
    , getAccountPasswordPolicyResponse
    , GetAccountPasswordPolicyResponse
    -- * Response Lenses
    , gapprsStatus
    , gapprsPasswordPolicy
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy =
    GetAccountPasswordPolicy'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountPasswordPolicy' with the minimum fields required to make a request.
--
getAccountPasswordPolicy
    :: GetAccountPasswordPolicy
getAccountPasswordPolicy = GetAccountPasswordPolicy'

instance AWSRequest GetAccountPasswordPolicy where
        type Rs GetAccountPasswordPolicy =
             GetAccountPasswordPolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "GetAccountPasswordPolicyResult"
              (\ s h x ->
                 GetAccountPasswordPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "PasswordPolicy"))

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

-- | Contains the response to a successful GetAccountPasswordPolicy request.
--
-- /See:/ 'getAccountPasswordPolicyResponse' smart constructor.
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
    { _gapprsStatus         :: !Int
    , _gapprsPasswordPolicy :: !PasswordPolicy
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountPasswordPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapprsStatus'
--
-- * 'gapprsPasswordPolicy'
getAccountPasswordPolicyResponse
    :: Int -- ^ 'gapprsStatus'
    -> PasswordPolicy -- ^ 'gapprsPasswordPolicy'
    -> GetAccountPasswordPolicyResponse
getAccountPasswordPolicyResponse pStatus_ pPasswordPolicy_ =
    GetAccountPasswordPolicyResponse'
    { _gapprsStatus = pStatus_
    , _gapprsPasswordPolicy = pPasswordPolicy_
    }

-- | The response status code.
gapprsStatus :: Lens' GetAccountPasswordPolicyResponse Int
gapprsStatus = lens _gapprsStatus (\ s a -> s{_gapprsStatus = a});

-- | Undocumented member.
gapprsPasswordPolicy :: Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprsPasswordPolicy = lens _gapprsPasswordPolicy (\ s a -> s{_gapprsPasswordPolicy = a});

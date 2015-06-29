{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
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

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountPasswordPolicy.html>
module Network.AWS.IAM.GetAccountPasswordPolicy
    (
    -- * Request
      GetAccountPasswordPolicy
    -- ** Request constructor
    , getAccountPasswordPolicy

    -- * Response
    , GetAccountPasswordPolicyResponse
    -- ** Response constructor
    , getAccountPasswordPolicyResponse
    -- ** Response lenses
    , gapprStatus
    , gapprPasswordPolicy
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy =
    GetAccountPasswordPolicy'
    deriving (Eq,Read,Show)

-- | 'GetAccountPasswordPolicy' smart constructor.
getAccountPasswordPolicy :: GetAccountPasswordPolicy
getAccountPasswordPolicy = GetAccountPasswordPolicy'

instance AWSRequest GetAccountPasswordPolicy where
        type Sv GetAccountPasswordPolicy = IAM
        type Rs GetAccountPasswordPolicy =
             GetAccountPasswordPolicyResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gapprStatus'
--
-- * 'gapprPasswordPolicy'
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
    { _gapprStatus         :: !Int
    , _gapprPasswordPolicy :: !PasswordPolicy
    } deriving (Eq,Read,Show)

-- | 'GetAccountPasswordPolicyResponse' smart constructor.
getAccountPasswordPolicyResponse :: Int -> PasswordPolicy -> GetAccountPasswordPolicyResponse
getAccountPasswordPolicyResponse pStatus pPasswordPolicy =
    GetAccountPasswordPolicyResponse'
    { _gapprStatus = pStatus
    , _gapprPasswordPolicy = pPasswordPolicy
    }

-- | FIXME: Undocumented member.
gapprStatus :: Lens' GetAccountPasswordPolicyResponse Int
gapprStatus = lens _gapprStatus (\ s a -> s{_gapprStatus = a});

-- | FIXME: Undocumented member.
gapprPasswordPolicy :: Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprPasswordPolicy = lens _gapprPasswordPolicy (\ s a -> s{_gapprPasswordPolicy = a});

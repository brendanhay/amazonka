{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
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

-- | Deletes the password policy for the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountPasswordPolicy.html>
module Network.AWS.IAM.DeleteAccountPasswordPolicy
    (
    -- * Request
      DeleteAccountPasswordPolicy
    -- ** Request constructor
    , deleteAccountPasswordPolicy

    -- * Response
    , DeleteAccountPasswordPolicyResponse
    -- ** Response constructor
    , deleteAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'deleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy' deriving (Eq, Read, Show)

-- | 'DeleteAccountPasswordPolicy' smart constructor.
deleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy';

instance AWSRequest DeleteAccountPasswordPolicy where
        type Sv DeleteAccountPasswordPolicy = IAM
        type Rs DeleteAccountPasswordPolicy =
             DeleteAccountPasswordPolicyResponse
        request = post
        response
          = receiveNull DeleteAccountPasswordPolicyResponse'

instance ToHeaders DeleteAccountPasswordPolicy where
        toHeaders = const mempty

instance ToPath DeleteAccountPasswordPolicy where
        toPath = const "/"

instance ToQuery DeleteAccountPasswordPolicy where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DeleteAccountPasswordPolicy" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | /See:/ 'deleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteAccountPasswordPolicyResponse' smart constructor.
deleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse';

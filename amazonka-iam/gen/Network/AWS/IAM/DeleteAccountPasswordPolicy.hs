{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountPasswordPolicy.html AWS API Reference> for DeleteAccountPasswordPolicy.
module Network.AWS.IAM.DeleteAccountPasswordPolicy
    (
    -- * Creating a Request
      DeleteAccountPasswordPolicy
    , deleteAccountPasswordPolicy

    -- * Destructuring the Response
    , DeleteAccountPasswordPolicyResponse
    , deleteAccountPasswordPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy =
    DeleteAccountPasswordPolicy'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAccountPasswordPolicy' smart constructor.
deleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'

instance AWSRequest DeleteAccountPasswordPolicy where
        type Sv DeleteAccountPasswordPolicy = IAM
        type Rs DeleteAccountPasswordPolicy =
             DeleteAccountPasswordPolicyResponse
        request = postQuery
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
data DeleteAccountPasswordPolicyResponse =
    DeleteAccountPasswordPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAccountPasswordPolicyResponse' smart constructor.
deleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'

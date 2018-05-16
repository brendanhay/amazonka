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
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account. There are no parameters.
--
--
module Network.AWS.IAM.DeleteAccountPasswordPolicy
    (
    -- * Creating a Request
      deleteAccountPasswordPolicy
    , DeleteAccountPasswordPolicy

    -- * Destructuring the Response
    , deleteAccountPasswordPolicyResponse
    , DeleteAccountPasswordPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy =
  DeleteAccountPasswordPolicy'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccountPasswordPolicy' with the minimum fields required to make a request.
--
deleteAccountPasswordPolicy
    :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'


instance AWSRequest DeleteAccountPasswordPolicy where
        type Rs DeleteAccountPasswordPolicy =
             DeleteAccountPasswordPolicyResponse
        request = postQuery iam
        response
          = receiveNull DeleteAccountPasswordPolicyResponse'

instance Hashable DeleteAccountPasswordPolicy where

instance NFData DeleteAccountPasswordPolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccountPasswordPolicyResponse' with the minimum fields required to make a request.
--
deleteAccountPasswordPolicyResponse
    :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'


instance NFData DeleteAccountPasswordPolicyResponse
         where

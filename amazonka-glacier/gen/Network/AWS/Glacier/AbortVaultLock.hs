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
-- Module      : Network.AWS.Glacier.AbortVaultLock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts the vault locking process if the vault lock is not in the @Locked@ state. If the vault lock is in the @Locked@ state when this operation is requested, the operation returns an @AccessDeniedException@ error. Aborting the vault locking process removes the vault lock policy from the specified vault.
--
--
-- A vault lock is put into the @InProgress@ state by calling 'InitiateVaultLock' . A vault lock is put into the @Locked@ state by calling 'CompleteVaultLock' . You can get the state of a vault lock by calling 'GetVaultLock' . For more information about the vault locking process, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> . For more information about vault lock policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies> .
--
-- This operation is idempotent. You can successfully invoke this operation multiple times, if the vault lock is in the @InProgress@ state or if there is no policy associated with the vault.
--
module Network.AWS.Glacier.AbortVaultLock
    (
    -- * Creating a Request
      abortVaultLock
    , AbortVaultLock
    -- * Request Lenses
    , avlAccountId
    , avlVaultName

    -- * Destructuring the Response
    , abortVaultLockResponse
    , AbortVaultLockResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input values for @AbortVaultLock@ .
--
--
--
-- /See:/ 'abortVaultLock' smart constructor.
data AbortVaultLock = AbortVaultLock'
  { _avlAccountId :: !Text
  , _avlVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortVaultLock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avlAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- * 'avlVaultName' - The name of the vault.
abortVaultLock
    :: Text -- ^ 'avlAccountId'
    -> Text -- ^ 'avlVaultName'
    -> AbortVaultLock
abortVaultLock pAccountId_ pVaultName_ =
  AbortVaultLock' {_avlAccountId = pAccountId_, _avlVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
avlAccountId :: Lens' AbortVaultLock Text
avlAccountId = lens _avlAccountId (\ s a -> s{_avlAccountId = a})

-- | The name of the vault.
avlVaultName :: Lens' AbortVaultLock Text
avlVaultName = lens _avlVaultName (\ s a -> s{_avlVaultName = a})

instance AWSRequest AbortVaultLock where
        type Rs AbortVaultLock = AbortVaultLockResponse
        request = delete glacier
        response = receiveNull AbortVaultLockResponse'

instance Hashable AbortVaultLock where

instance NFData AbortVaultLock where

instance ToHeaders AbortVaultLock where
        toHeaders = const mempty

instance ToPath AbortVaultLock where
        toPath AbortVaultLock'{..}
          = mconcat
              ["/", toBS _avlAccountId, "/vaults/",
               toBS _avlVaultName, "/lock-policy"]

instance ToQuery AbortVaultLock where
        toQuery = const mempty

-- | /See:/ 'abortVaultLockResponse' smart constructor.
data AbortVaultLockResponse =
  AbortVaultLockResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortVaultLockResponse' with the minimum fields required to make a request.
--
abortVaultLockResponse
    :: AbortVaultLockResponse
abortVaultLockResponse = AbortVaultLockResponse'


instance NFData AbortVaultLockResponse where

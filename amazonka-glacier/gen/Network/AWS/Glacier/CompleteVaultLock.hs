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
-- Module      : Network.AWS.Glacier.CompleteVaultLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation completes the vault locking process by transitioning the
-- vault lock from the 'InProgress' state to the 'Locked' state, which
-- causes the vault lock policy to become unchangeable. A vault lock is put
-- into the 'InProgress' state by calling InitiateVaultLock. You can obtain
-- the state of the vault lock by calling GetVaultLock. For more
-- information about the vault locking process,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- This operation is idempotent. This request is always successful if the
-- vault lock is in the 'Locked' state and the provided lock ID matches the
-- lock ID originally used to lock the vault.
--
-- If an invalid lock ID is passed in the request when the vault lock is in
-- the 'Locked' state, the operation returns an 'AccessDeniedException'
-- error. If an invalid lock ID is passed in the request when the vault
-- lock is in the 'InProgress' state, the operation throws an
-- 'InvalidParameter' error.
--
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-CompleteVaultLock.html AWS API Reference> for CompleteVaultLock.
module Network.AWS.Glacier.CompleteVaultLock
    (
    -- * Creating a Request
      completeVaultLock
    , CompleteVaultLock
    -- * Request Lenses
    , cvlAccountId
    , cvlVaultName
    , cvlLockId

    -- * Destructuring the Response
    , completeVaultLockResponse
    , CompleteVaultLockResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input values for 'CompleteVaultLock'.
--
-- /See:/ 'completeVaultLock' smart constructor.
data CompleteVaultLock = CompleteVaultLock'
    { _cvlAccountId :: !Text
    , _cvlVaultName :: !Text
    , _cvlLockId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompleteVaultLock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvlAccountId'
--
-- * 'cvlVaultName'
--
-- * 'cvlLockId'
completeVaultLock
    :: Text -- ^ 'cvlAccountId'
    -> Text -- ^ 'cvlVaultName'
    -> Text -- ^ 'cvlLockId'
    -> CompleteVaultLock
completeVaultLock pAccountId_ pVaultName_ pLockId_ =
    CompleteVaultLock'
    { _cvlAccountId = pAccountId_
    , _cvlVaultName = pVaultName_
    , _cvlLockId = pLockId_
    }

-- | The 'AccountId' value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos'-'apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your account ID, do not include any hyphens (apos-apos) in the
-- ID.
cvlAccountId :: Lens' CompleteVaultLock Text
cvlAccountId = lens _cvlAccountId (\ s a -> s{_cvlAccountId = a});

-- | The name of the vault.
cvlVaultName :: Lens' CompleteVaultLock Text
cvlVaultName = lens _cvlVaultName (\ s a -> s{_cvlVaultName = a});

-- | The 'lockId' value is the lock ID obtained from a InitiateVaultLock
-- request.
cvlLockId :: Lens' CompleteVaultLock Text
cvlLockId = lens _cvlLockId (\ s a -> s{_cvlLockId = a});

instance AWSRequest CompleteVaultLock where
        type Rs CompleteVaultLock = CompleteVaultLockResponse
        request = postJSON glacier
        response = receiveNull CompleteVaultLockResponse'

instance ToHeaders CompleteVaultLock where
        toHeaders = const mempty

instance ToJSON CompleteVaultLock where
        toJSON = const (Object mempty)

instance ToPath CompleteVaultLock where
        toPath CompleteVaultLock'{..}
          = mconcat
              ["/", toBS _cvlAccountId, "/vaults/",
               toBS _cvlVaultName, "/lock-policy/", toBS _cvlLockId]

instance ToQuery CompleteVaultLock where
        toQuery = const mempty

-- | /See:/ 'completeVaultLockResponse' smart constructor.
data CompleteVaultLockResponse =
    CompleteVaultLockResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompleteVaultLockResponse' with the minimum fields required to make a request.
--
completeVaultLockResponse
    :: CompleteVaultLockResponse
completeVaultLockResponse = CompleteVaultLockResponse'

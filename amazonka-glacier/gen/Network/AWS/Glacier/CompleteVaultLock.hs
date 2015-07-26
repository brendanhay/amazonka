{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CompleteVaultLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation completes the vault locking process by transitioning the
-- vault lock from the @InProgress@ state to the @Locked@ state, which
-- causes the vault lock policy to become unchangeable. A vault lock is put
-- into the @InProgress@ state by calling InitiateVaultLock. You can obtain
-- the state of the vault lock by calling GetVaultLock. For more
-- information about the vault locking process,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- This operation is idempotent. This request is always successful if the
-- vault lock is in the @Locked@ state and the provided lock ID matches the
-- lock ID originally used to lock the vault.
--
-- If an invalid lock ID is passed in the request when the vault lock is in
-- the @Locked@ state, the operation returns an @AccessDeniedException@
-- error. If an invalid lock ID is passed in the request when the vault
-- lock is in the @InProgress@ state, the operation throws an
-- @InvalidParameter@ error.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-CompleteVaultLock.html>
module Network.AWS.Glacier.CompleteVaultLock
    (
    -- * Request
      CompleteVaultLock
    -- ** Request constructor
    , completeVaultLock
    -- ** Request lenses
    , cvlAccountId
    , cvlVaultName
    , cvlLockId

    -- * Response
    , CompleteVaultLockResponse
    -- ** Response constructor
    , completeVaultLockResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input values for @CompleteVaultLock@.
--
-- /See:/ 'completeVaultLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvlAccountId'
--
-- * 'cvlVaultName'
--
-- * 'cvlLockId'
data CompleteVaultLock = CompleteVaultLock'
    { _cvlAccountId :: !Text
    , _cvlVaultName :: !Text
    , _cvlLockId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteVaultLock' smart constructor.
completeVaultLock :: Text -> Text -> Text -> CompleteVaultLock
completeVaultLock pAccountId_ pVaultName_ pLockId_ =
    CompleteVaultLock'
    { _cvlAccountId = pAccountId_
    , _cvlVaultName = pVaultName_
    , _cvlLockId = pLockId_
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your account ID, do not include any hyphens (apos-apos) in the
-- ID.
cvlAccountId :: Lens' CompleteVaultLock Text
cvlAccountId = lens _cvlAccountId (\ s a -> s{_cvlAccountId = a});

-- | The name of the vault.
cvlVaultName :: Lens' CompleteVaultLock Text
cvlVaultName = lens _cvlVaultName (\ s a -> s{_cvlVaultName = a});

-- | The @lockId@ value is the lock ID obtained from a InitiateVaultLock
-- request.
cvlLockId :: Lens' CompleteVaultLock Text
cvlLockId = lens _cvlLockId (\ s a -> s{_cvlLockId = a});

instance AWSRequest CompleteVaultLock where
        type Sv CompleteVaultLock = Glacier
        type Rs CompleteVaultLock = CompleteVaultLockResponse
        request = postJSON
        response = receiveNull CompleteVaultLockResponse'

instance ToHeaders CompleteVaultLock where
        toHeaders = const mempty

instance ToJSON CompleteVaultLock where
        toJSON = const (Object mempty)

instance ToPath CompleteVaultLock where
        toPath CompleteVaultLock'{..}
          = mconcat
              ["/", toPath _cvlAccountId, "/vaults/",
               toPath _cvlVaultName, "/lock-policy/",
               toPath _cvlLockId]

instance ToQuery CompleteVaultLock where
        toQuery = const mempty

-- | /See:/ 'completeVaultLockResponse' smart constructor.
data CompleteVaultLockResponse =
    CompleteVaultLockResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteVaultLockResponse' smart constructor.
completeVaultLockResponse :: CompleteVaultLockResponse
completeVaultLockResponse = CompleteVaultLockResponse'

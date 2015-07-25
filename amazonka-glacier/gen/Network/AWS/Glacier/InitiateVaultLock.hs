{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateVaultLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the vault locking process by doing the
-- following:
--
-- -   Installing a vault lock policy on the specified vault.
--
-- -   Setting the lock state of vault lock to @InProgress@.
--
-- -   Returning a lock ID, which is used to complete the vault locking
--     process.
--
-- You can set one vault lock policy for each vault and this policy can be
-- up to 20 KB in size. For more information about vault lock policies, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies>.
--
-- You must complete the vault locking process within 24 hours after the
-- vault lock enters the @InProgress@ state. After the 24 hour window ends,
-- the lock ID expires, the vault automatically exits the @InProgress@
-- state, and the vault lock policy is removed from the vault. You call
-- CompleteVaultLock to complete the vault locking process by setting the
-- state of the vault lock to @Locked@.
--
-- After a vault lock is in the @Locked@ state, you cannot initiate a new
-- vault lock for the vault.
--
-- You can abort the vault locking process by calling AbortVaultLock. You
-- can get the state of the vault lock by calling GetVaultLock. For more
-- information about the vault locking process,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- If this operation is called when the vault lock is in the @InProgress@
-- state, the operation returns an @AccessDeniedException@ error. When the
-- vault lock is in the @InProgress@ state you must call AbortVaultLock
-- before you can initiate a new vault lock policy.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-InitiateVaultLock.html>
module Network.AWS.Glacier.InitiateVaultLock
    (
    -- * Request
      InitiateVaultLock
    -- ** Request constructor
    , initiateVaultLock
    -- ** Request lenses
    , ivlPolicy
    , ivlAccountId
    , ivlVaultName

    -- * Response
    , InitiateVaultLockResponse
    -- ** Response constructor
    , initiateVaultLockResponse
    -- ** Response lenses
    , ivlrsLockId
    , ivlrsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input values for @InitiateVaultLock@.
--
-- /See:/ 'initiateVaultLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivlPolicy'
--
-- * 'ivlAccountId'
--
-- * 'ivlVaultName'
data InitiateVaultLock = InitiateVaultLock'
    { _ivlPolicy    :: !(Maybe VaultLockPolicy)
    , _ivlAccountId :: !Text
    , _ivlVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InitiateVaultLock' smart constructor.
initiateVaultLock :: Text -> Text -> InitiateVaultLock
initiateVaultLock pAccountId_ pVaultName_ =
    InitiateVaultLock'
    { _ivlPolicy = Nothing
    , _ivlAccountId = pAccountId_
    , _ivlVaultName = pVaultName_
    }

-- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
ivlPolicy :: Lens' InitiateVaultLock (Maybe VaultLockPolicy)
ivlPolicy = lens _ivlPolicy (\ s a -> s{_ivlPolicy = a});

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your account ID, do not include any hyphens (apos-apos) in the
-- ID.
ivlAccountId :: Lens' InitiateVaultLock Text
ivlAccountId = lens _ivlAccountId (\ s a -> s{_ivlAccountId = a});

-- | The name of the vault.
ivlVaultName :: Lens' InitiateVaultLock Text
ivlVaultName = lens _ivlVaultName (\ s a -> s{_ivlVaultName = a});

instance AWSRequest InitiateVaultLock where
        type Sv InitiateVaultLock = Glacier
        type Rs InitiateVaultLock = InitiateVaultLockResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 InitiateVaultLockResponse' <$>
                   (h .#? "x-amz-lock-id") <*> (pure (fromEnum s)))

instance ToHeaders InitiateVaultLock where
        toHeaders = const mempty

instance ToJSON InitiateVaultLock where
        toJSON InitiateVaultLock'{..}
          = object ["policy" .= _ivlPolicy]

instance ToPath InitiateVaultLock where
        toPath InitiateVaultLock'{..}
          = mconcat
              ["/", toText _ivlAccountId, "/vaults/",
               toText _ivlVaultName, "/lock-policy"]

instance ToQuery InitiateVaultLock where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'initiateVaultLockResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivlrsLockId'
--
-- * 'ivlrsStatus'
data InitiateVaultLockResponse = InitiateVaultLockResponse'
    { _ivlrsLockId :: !(Maybe Text)
    , _ivlrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InitiateVaultLockResponse' smart constructor.
initiateVaultLockResponse :: Int -> InitiateVaultLockResponse
initiateVaultLockResponse pStatus_ =
    InitiateVaultLockResponse'
    { _ivlrsLockId = Nothing
    , _ivlrsStatus = pStatus_
    }

-- | The lock ID, which is used to complete the vault locking process.
ivlrsLockId :: Lens' InitiateVaultLockResponse (Maybe Text)
ivlrsLockId = lens _ivlrsLockId (\ s a -> s{_ivlrsLockId = a});

-- | FIXME: Undocumented member.
ivlrsStatus :: Lens' InitiateVaultLockResponse Int
ivlrsStatus = lens _ivlrsStatus (\ s a -> s{_ivlrsStatus = a});

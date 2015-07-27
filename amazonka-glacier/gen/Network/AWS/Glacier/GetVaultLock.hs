{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the following attributes from the @lock-policy@
-- subresource set on the specified vault:
--
-- -   The vault lock policy set on the vault.
--
-- -   The state of the vault lock, which is either @InProgess@ or
--     @Locked@.
--
-- -   When the lock ID expires. The lock ID is used to complete the vault
--     locking process.
--
-- -   When the vault lock was initiated and put into the @InProgress@
--     state.
--
-- A vault lock is put into the @InProgress@ state by calling
-- InitiateVaultLock. A vault lock is put into the @Locked@ state by
-- calling CompleteVaultLock. You can abort the vault locking process by
-- calling AbortVaultLock. For more information about the vault locking
-- process,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- If there is no vault lock policy set on the vault, the operation returns
-- a @404 Not found@ error. For more information about vault lock policies,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetVaultLock.html>
module Network.AWS.Glacier.GetVaultLock
    (
    -- * Request
      GetVaultLock
    -- ** Request constructor
    , getVaultLock
    -- ** Request lenses
    , gvlAccountId
    , gvlVaultName

    -- * Response
    , GetVaultLockResponse
    -- ** Response constructor
    , getVaultLockResponse
    -- ** Response lenses
    , gvlrsState
    , gvlrsExpirationDate
    , gvlrsCreationDate
    , gvlrsPolicy
    , gvlrsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input values for @GetVaultLock@.
--
-- /See:/ 'getVaultLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvlAccountId'
--
-- * 'gvlVaultName'
data GetVaultLock = GetVaultLock'
    { _gvlAccountId :: !Text
    , _gvlVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultLock' smart constructor.
getVaultLock :: Text -> Text -> GetVaultLock
getVaultLock pAccountId_ pVaultName_ =
    GetVaultLock'
    { _gvlAccountId = pAccountId_
    , _gvlVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
gvlAccountId :: Lens' GetVaultLock Text
gvlAccountId = lens _gvlAccountId (\ s a -> s{_gvlAccountId = a});

-- | The name of the vault.
gvlVaultName :: Lens' GetVaultLock Text
gvlVaultName = lens _gvlVaultName (\ s a -> s{_gvlVaultName = a});

instance AWSRequest GetVaultLock where
        type Sv GetVaultLock = Glacier
        type Rs GetVaultLock = GetVaultLockResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetVaultLockResponse' <$>
                   (x .?> "State") <*> (x .?> "ExpirationDate") <*>
                     (x .?> "CreationDate")
                     <*> (x .?> "Policy")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetVaultLock where
        toHeaders = const mempty

instance ToPath GetVaultLock where
        toPath GetVaultLock'{..}
          = mconcat
              ["/", toPath _gvlAccountId, "/vaults/",
               toPath _gvlVaultName, "/lock-policy"]

instance ToQuery GetVaultLock where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'getVaultLockResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvlrsState'
--
-- * 'gvlrsExpirationDate'
--
-- * 'gvlrsCreationDate'
--
-- * 'gvlrsPolicy'
--
-- * 'gvlrsStatus'
data GetVaultLockResponse = GetVaultLockResponse'
    { _gvlrsState          :: !(Maybe Text)
    , _gvlrsExpirationDate :: !(Maybe Text)
    , _gvlrsCreationDate   :: !(Maybe Text)
    , _gvlrsPolicy         :: !(Maybe Text)
    , _gvlrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultLockResponse' smart constructor.
getVaultLockResponse :: Int -> GetVaultLockResponse
getVaultLockResponse pStatus_ =
    GetVaultLockResponse'
    { _gvlrsState = Nothing
    , _gvlrsExpirationDate = Nothing
    , _gvlrsCreationDate = Nothing
    , _gvlrsPolicy = Nothing
    , _gvlrsStatus = pStatus_
    }

-- | The state of the vault lock. @InProgress@ or @Locked@.
gvlrsState :: Lens' GetVaultLockResponse (Maybe Text)
gvlrsState = lens _gvlrsState (\ s a -> s{_gvlrsState = a});

-- | The UTC date and time at which the lock ID expires. This value can be
-- @null@ if the vault lock is in a @Locked@ state.
gvlrsExpirationDate :: Lens' GetVaultLockResponse (Maybe Text)
gvlrsExpirationDate = lens _gvlrsExpirationDate (\ s a -> s{_gvlrsExpirationDate = a});

-- | The UTC date and time at which the vault lock was put into the
-- @InProgress@ state.
gvlrsCreationDate :: Lens' GetVaultLockResponse (Maybe Text)
gvlrsCreationDate = lens _gvlrsCreationDate (\ s a -> s{_gvlrsCreationDate = a});

-- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
gvlrsPolicy :: Lens' GetVaultLockResponse (Maybe Text)
gvlrsPolicy = lens _gvlrsPolicy (\ s a -> s{_gvlrsPolicy = a});

-- | FIXME: Undocumented member.
gvlrsStatus :: Lens' GetVaultLockResponse Int
gvlrsStatus = lens _gvlrsStatus (\ s a -> s{_gvlrsStatus = a});

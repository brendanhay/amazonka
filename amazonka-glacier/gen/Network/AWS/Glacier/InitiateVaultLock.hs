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
-- Module      : Network.AWS.Glacier.InitiateVaultLock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the vault locking process by doing the following:
--
--
--     * Installing a vault lock policy on the specified vault.
--
--     * Setting the lock state of vault lock to @InProgress@ .
--
--     * Returning a lock ID, which is used to complete the vault locking process.
--
--
--
-- You can set one vault lock policy for each vault and this policy can be up to 20 KB in size. For more information about vault lock policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies> .
--
-- You must complete the vault locking process within 24 hours after the vault lock enters the @InProgress@ state. After the 24 hour window ends, the lock ID expires, the vault automatically exits the @InProgress@ state, and the vault lock policy is removed from the vault. You call 'CompleteVaultLock' to complete the vault locking process by setting the state of the vault lock to @Locked@ .
--
-- After a vault lock is in the @Locked@ state, you cannot initiate a new vault lock for the vault.
--
-- You can abort the vault locking process by calling 'AbortVaultLock' . You can get the state of the vault lock by calling 'GetVaultLock' . For more information about the vault locking process, <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> .
--
-- If this operation is called when the vault lock is in the @InProgress@ state, the operation returns an @AccessDeniedException@ error. When the vault lock is in the @InProgress@ state you must call 'AbortVaultLock' before you can initiate a new vault lock policy.
--
module Network.AWS.Glacier.InitiateVaultLock
    (
    -- * Creating a Request
      initiateVaultLock
    , InitiateVaultLock
    -- * Request Lenses
    , ivlPolicy
    , ivlAccountId
    , ivlVaultName

    -- * Destructuring the Response
    , initiateVaultLockResponse
    , InitiateVaultLockResponse
    -- * Response Lenses
    , ivlrsLockId
    , ivlrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input values for @InitiateVaultLock@ .
--
--
--
-- /See:/ 'initiateVaultLock' smart constructor.
data InitiateVaultLock = InitiateVaultLock'
  { _ivlPolicy    :: !(Maybe VaultLockPolicy)
  , _ivlAccountId :: !Text
  , _ivlVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateVaultLock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivlPolicy' - The vault lock policy as a JSON string, which uses "\" as an escape character.
--
-- * 'ivlAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- * 'ivlVaultName' - The name of the vault.
initiateVaultLock
    :: Text -- ^ 'ivlAccountId'
    -> Text -- ^ 'ivlVaultName'
    -> InitiateVaultLock
initiateVaultLock pAccountId_ pVaultName_ =
  InitiateVaultLock'
    { _ivlPolicy = Nothing
    , _ivlAccountId = pAccountId_
    , _ivlVaultName = pVaultName_
    }


-- | The vault lock policy as a JSON string, which uses "\" as an escape character.
ivlPolicy :: Lens' InitiateVaultLock (Maybe VaultLockPolicy)
ivlPolicy = lens _ivlPolicy (\ s a -> s{_ivlPolicy = a})

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
ivlAccountId :: Lens' InitiateVaultLock Text
ivlAccountId = lens _ivlAccountId (\ s a -> s{_ivlAccountId = a})

-- | The name of the vault.
ivlVaultName :: Lens' InitiateVaultLock Text
ivlVaultName = lens _ivlVaultName (\ s a -> s{_ivlVaultName = a})

instance AWSRequest InitiateVaultLock where
        type Rs InitiateVaultLock = InitiateVaultLockResponse
        request = postJSON glacier
        response
          = receiveEmpty
              (\ s h x ->
                 InitiateVaultLockResponse' <$>
                   (h .#? "x-amz-lock-id") <*> (pure (fromEnum s)))

instance Hashable InitiateVaultLock where

instance NFData InitiateVaultLock where

instance ToHeaders InitiateVaultLock where
        toHeaders = const mempty

instance ToJSON InitiateVaultLock where
        toJSON InitiateVaultLock'{..}
          = object (catMaybes [("policy" .=) <$> _ivlPolicy])

instance ToPath InitiateVaultLock where
        toPath InitiateVaultLock'{..}
          = mconcat
              ["/", toBS _ivlAccountId, "/vaults/",
               toBS _ivlVaultName, "/lock-policy"]

instance ToQuery InitiateVaultLock where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'initiateVaultLockResponse' smart constructor.
data InitiateVaultLockResponse = InitiateVaultLockResponse'
  { _ivlrsLockId         :: !(Maybe Text)
  , _ivlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateVaultLockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivlrsLockId' - The lock ID, which is used to complete the vault locking process.
--
-- * 'ivlrsResponseStatus' - -- | The response status code.
initiateVaultLockResponse
    :: Int -- ^ 'ivlrsResponseStatus'
    -> InitiateVaultLockResponse
initiateVaultLockResponse pResponseStatus_ =
  InitiateVaultLockResponse'
    {_ivlrsLockId = Nothing, _ivlrsResponseStatus = pResponseStatus_}


-- | The lock ID, which is used to complete the vault locking process.
ivlrsLockId :: Lens' InitiateVaultLockResponse (Maybe Text)
ivlrsLockId = lens _ivlrsLockId (\ s a -> s{_ivlrsLockId = a})

-- | -- | The response status code.
ivlrsResponseStatus :: Lens' InitiateVaultLockResponse Int
ivlrsResponseStatus = lens _ivlrsResponseStatus (\ s a -> s{_ivlrsResponseStatus = a})

instance NFData InitiateVaultLockResponse where

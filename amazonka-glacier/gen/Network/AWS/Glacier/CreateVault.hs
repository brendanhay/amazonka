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
-- Module      : Network.AWS.Glacier.CreateVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a new vault with the specified name. The name of the vault must be unique within a region for an AWS account. You can create up to 1,000 vaults per account. If you need to create more vaults, contact Amazon Glacier.
--
--
-- You must use the following guidelines when naming a vault.
--
--     * Names can be between 1 and 255 characters long.
--
--     * Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), and '.' (period).
--
--
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html Creating a Vault in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html Create Vault > in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.CreateVault
    (
    -- * Creating a Request
      createVault
    , CreateVault
    -- * Request Lenses
    , cvAccountId
    , cvVaultName

    -- * Destructuring the Response
    , createVaultResponse
    , CreateVaultResponse
    -- * Response Lenses
    , cvrsLocation
    , cvrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options to create a vault.
--
--
--
-- /See:/ 'createVault' smart constructor.
data CreateVault = CreateVault'
  { _cvAccountId :: !Text
  , _cvVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- * 'cvVaultName' - The name of the vault.
createVault
    :: Text -- ^ 'cvAccountId'
    -> Text -- ^ 'cvVaultName'
    -> CreateVault
createVault pAccountId_ pVaultName_ =
  CreateVault' {_cvAccountId = pAccountId_, _cvVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
cvAccountId :: Lens' CreateVault Text
cvAccountId = lens _cvAccountId (\ s a -> s{_cvAccountId = a})

-- | The name of the vault.
cvVaultName :: Lens' CreateVault Text
cvVaultName = lens _cvVaultName (\ s a -> s{_cvVaultName = a})

instance AWSRequest CreateVault where
        type Rs CreateVault = CreateVaultResponse
        request = putJSON glacier
        response
          = receiveEmpty
              (\ s h x ->
                 CreateVaultResponse' <$>
                   (h .#? "Location") <*> (pure (fromEnum s)))

instance Hashable CreateVault where

instance NFData CreateVault where

instance ToHeaders CreateVault where
        toHeaders = const mempty

instance ToJSON CreateVault where
        toJSON = const (Object mempty)

instance ToPath CreateVault where
        toPath CreateVault'{..}
          = mconcat
              ["/", toBS _cvAccountId, "/vaults/",
               toBS _cvVaultName]

instance ToQuery CreateVault where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'createVaultResponse' smart constructor.
data CreateVaultResponse = CreateVaultResponse'
  { _cvrsLocation       :: !(Maybe Text)
  , _cvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrsLocation' - The URI of the vault that was created.
--
-- * 'cvrsResponseStatus' - -- | The response status code.
createVaultResponse
    :: Int -- ^ 'cvrsResponseStatus'
    -> CreateVaultResponse
createVaultResponse pResponseStatus_ =
  CreateVaultResponse'
    {_cvrsLocation = Nothing, _cvrsResponseStatus = pResponseStatus_}


-- | The URI of the vault that was created.
cvrsLocation :: Lens' CreateVaultResponse (Maybe Text)
cvrsLocation = lens _cvrsLocation (\ s a -> s{_cvrsLocation = a})

-- | -- | The response status code.
cvrsResponseStatus :: Lens' CreateVaultResponse Int
cvrsResponseStatus = lens _cvrsResponseStatus (\ s a -> s{_cvrsResponseStatus = a})

instance NFData CreateVaultResponse where

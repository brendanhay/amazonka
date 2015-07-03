{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.CreateVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation creates a new vault with the specified name. The name of
-- the vault must be unique within a region for an AWS account. You can
-- create up to 1,000 vaults per account. If you need to create more
-- vaults, contact Amazon Glacier.
--
-- You must use the following guidelines when naming a vault.
--
-- -   Names can be between 1 and 255 characters long.
--
-- -   Allowed characters are a-z, A-Z, 0-9, \'_\' (underscore), \'-\'
--     (hyphen), and \'.\' (period).
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html Creating a Vault in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html Create Vault>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-CreateVault.html>
module Network.AWS.Glacier.CreateVault
    (
    -- * Request
      CreateVault
    -- ** Request constructor
    , createVault
    -- ** Request lenses
    , cvAccountId
    , cvVaultName

    -- * Response
    , CreateVaultResponse
    -- ** Response constructor
    , createVaultResponse
    -- ** Response lenses
    , cvrLocation
    , cvrStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to create a vault.
--
-- /See:/ 'createVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvAccountId'
--
-- * 'cvVaultName'
data CreateVault = CreateVault'
    { _cvAccountId :: !Text
    , _cvVaultName :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateVault' smart constructor.
createVault :: Text -> Text -> CreateVault
createVault pAccountId pVaultName =
    CreateVault'
    { _cvAccountId = pAccountId
    , _cvVaultName = pVaultName
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your Account ID, do not include any hyphens (apos-apos) in the
-- ID.
cvAccountId :: Lens' CreateVault Text
cvAccountId = lens _cvAccountId (\ s a -> s{_cvAccountId = a});

-- | The name of the vault.
cvVaultName :: Lens' CreateVault Text
cvVaultName = lens _cvVaultName (\ s a -> s{_cvVaultName = a});

instance AWSRequest CreateVault where
        type Sv CreateVault = Glacier
        type Rs CreateVault = CreateVaultResponse
        request = putJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateVaultResponse' <$>
                   (h .#? "Location") <*> (pure (fromEnum s)))

instance ToHeaders CreateVault where
        toHeaders = const mempty

instance ToJSON CreateVault where
        toJSON = const (Object mempty)

instance ToPath CreateVault where
        toPath CreateVault'{..}
          = mconcat
              ["/", toText _cvAccountId, "/vaults/",
               toText _cvVaultName]

instance ToQuery CreateVault where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'createVaultResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrLocation'
--
-- * 'cvrStatus'
data CreateVaultResponse = CreateVaultResponse'
    { _cvrLocation :: !(Maybe Text)
    , _cvrStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateVaultResponse' smart constructor.
createVaultResponse :: Int -> CreateVaultResponse
createVaultResponse pStatus =
    CreateVaultResponse'
    { _cvrLocation = Nothing
    , _cvrStatus = pStatus
    }

-- | The URI of the vault that was created.
cvrLocation :: Lens' CreateVaultResponse (Maybe Text)
cvrLocation = lens _cvrLocation (\ s a -> s{_cvrLocation = a});

-- | FIXME: Undocumented member.
cvrStatus :: Lens' CreateVaultResponse Int
cvrStatus = lens _cvrStatus (\ s a -> s{_cvrStatus = a});

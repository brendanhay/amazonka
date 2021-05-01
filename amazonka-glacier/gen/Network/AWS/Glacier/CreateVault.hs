{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CreateVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a new vault with the specified name. The name of
-- the vault must be unique within a region for an AWS account. You can
-- create up to 1,000 vaults per account. If you need to create more
-- vaults, contact Amazon S3 Glacier.
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
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html Creating a Vault in Amazon Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html Create Vault>
-- in the /Amazon Glacier Developer Guide/.
module Network.AWS.Glacier.CreateVault
  ( -- * Creating a Request
    CreateVault (..),
    newCreateVault,

    -- * Request Lenses
    createVault_accountId,
    createVault_vaultName,

    -- * Destructuring the Response
    CreateVaultResponse (..),
    newCreateVaultResponse,

    -- * Response Lenses
    createVaultResponse_location,
    createVaultResponse_httpStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to create a vault.
--
-- /See:/ 'newCreateVault' smart constructor.
data CreateVault = CreateVault'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon S3 Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createVault_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon S3 Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'createVault_vaultName' - The name of the vault.
newCreateVault ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  CreateVault
newCreateVault pAccountId_ pVaultName_ =
  CreateVault'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon S3 Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
createVault_accountId :: Lens.Lens' CreateVault Prelude.Text
createVault_accountId = Lens.lens (\CreateVault' {accountId} -> accountId) (\s@CreateVault' {} a -> s {accountId = a} :: CreateVault)

-- | The name of the vault.
createVault_vaultName :: Lens.Lens' CreateVault Prelude.Text
createVault_vaultName = Lens.lens (\CreateVault' {vaultName} -> vaultName) (\s@CreateVault' {} a -> s {vaultName = a} :: CreateVault)

instance Prelude.AWSRequest CreateVault where
  type Rs CreateVault = CreateVaultResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateVaultResponse'
            Prelude.<$> (h Prelude..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVault

instance Prelude.NFData CreateVault

instance Prelude.ToHeaders CreateVault where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateVault where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath CreateVault where
  toPath CreateVault' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName
      ]

instance Prelude.ToQuery CreateVault where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newCreateVaultResponse' smart constructor.
data CreateVaultResponse = CreateVaultResponse'
  { -- | The URI of the vault that was created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createVaultResponse_location' - The URI of the vault that was created.
--
-- 'httpStatus', 'createVaultResponse_httpStatus' - The response's http status code.
newCreateVaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVaultResponse
newCreateVaultResponse pHttpStatus_ =
  CreateVaultResponse'
    { location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URI of the vault that was created.
createVaultResponse_location :: Lens.Lens' CreateVaultResponse (Prelude.Maybe Prelude.Text)
createVaultResponse_location = Lens.lens (\CreateVaultResponse' {location} -> location) (\s@CreateVaultResponse' {} a -> s {location = a} :: CreateVaultResponse)

-- | The response's http status code.
createVaultResponse_httpStatus :: Lens.Lens' CreateVaultResponse Prelude.Int
createVaultResponse_httpStatus = Lens.lens (\CreateVaultResponse' {httpStatus} -> httpStatus) (\s@CreateVaultResponse' {} a -> s {httpStatus = a} :: CreateVaultResponse)

instance Prelude.NFData CreateVaultResponse

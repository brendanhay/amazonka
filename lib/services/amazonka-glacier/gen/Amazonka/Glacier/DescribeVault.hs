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
-- Module      : Amazonka.Glacier.DescribeVault
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a vault, including the vault\'s
-- Amazon Resource Name (ARN), the date the vault was created, the number
-- of archives it contains, and the total size of all the archives in the
-- vault. The number of archives and their total size are as of the last
-- inventory generation. This means that if you add or remove an archive
-- from a vault, and then immediately use Describe Vault, the change in
-- contents will not be immediately reflected. If you want to retrieve the
-- latest inventory of the vault, use InitiateJob. Amazon S3 Glacier
-- generates vault inventories approximately daily. For more information,
-- see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory in Amazon S3 Glacier>.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon S3 Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html Describe Vault>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.DescribeVault
  ( -- * Creating a Request
    DescribeVault (..),
    newDescribeVault,

    -- * Request Lenses
    describeVault_accountId,
    describeVault_vaultName,

    -- * Destructuring the Response
    DescribeVaultOutput (..),
    newDescribeVaultOutput,

    -- * Response Lenses
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_creationDate,
    describeVaultOutput_vaultName,
    describeVaultOutput_numberOfArchives,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_vaultARN,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for retrieving metadata for a specific vault in Amazon
-- Glacier.
--
-- /See:/ 'newDescribeVault' smart constructor.
data DescribeVault = DescribeVault'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeVault_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'describeVault_vaultName' - The name of the vault.
newDescribeVault ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  DescribeVault
newDescribeVault pAccountId_ pVaultName_ =
  DescribeVault'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
describeVault_accountId :: Lens.Lens' DescribeVault Prelude.Text
describeVault_accountId = Lens.lens (\DescribeVault' {accountId} -> accountId) (\s@DescribeVault' {} a -> s {accountId = a} :: DescribeVault)

-- | The name of the vault.
describeVault_vaultName :: Lens.Lens' DescribeVault Prelude.Text
describeVault_vaultName = Lens.lens (\DescribeVault' {vaultName} -> vaultName) (\s@DescribeVault' {} a -> s {vaultName = a} :: DescribeVault)

instance Core.AWSRequest DescribeVault where
  type AWSResponse DescribeVault = DescribeVaultOutput
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeVault where
  hashWithSalt _salt DescribeVault' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData DescribeVault where
  rnf DescribeVault' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Data.ToHeaders DescribeVault where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVault where
  toPath DescribeVault' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName
      ]

instance Data.ToQuery DescribeVault where
  toQuery = Prelude.const Prelude.mempty

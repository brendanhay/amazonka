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
-- Module      : Network.AWS.Glacier.DescribeVault
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glacier.DescribeVault
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
    describeVaultOutput_vaultARN,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_numberOfArchives,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    accountId :: Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
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
describeVault_accountId :: Lens.Lens' DescribeVault Core.Text
describeVault_accountId = Lens.lens (\DescribeVault' {accountId} -> accountId) (\s@DescribeVault' {} a -> s {accountId = a} :: DescribeVault)

-- | The name of the vault.
describeVault_vaultName :: Lens.Lens' DescribeVault Core.Text
describeVault_vaultName = Lens.lens (\DescribeVault' {vaultName} -> vaultName) (\s@DescribeVault' {} a -> s {vaultName = a} :: DescribeVault)

instance Core.AWSRequest DescribeVault where
  type AWSResponse DescribeVault = DescribeVaultOutput
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DescribeVault

instance Core.NFData DescribeVault

instance Core.ToHeaders DescribeVault where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVault where
  toPath DescribeVault' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName
      ]

instance Core.ToQuery DescribeVault where
  toQuery = Core.const Core.mempty

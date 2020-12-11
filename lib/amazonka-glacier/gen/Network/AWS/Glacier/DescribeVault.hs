{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a vault, including the vault's Amazon Resource Name (ARN), the date the vault was created, the number of archives it contains, and the total size of all the archives in the vault. The number of archives and their total size are as of the last inventory generation. This means that if you add or remove an archive from a vault, and then immediately use Describe Vault, the change in contents will not be immediately reflected. If you want to retrieve the latest inventory of the vault, use 'InitiateJob' . Amazon S3 Glacier generates vault inventories approximately daily. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory in Amazon S3 Glacier> .
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html Describe Vault > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.DescribeVault
  ( -- * Creating a request
    DescribeVault (..),
    mkDescribeVault,

    -- ** Request lenses
    dvAccountId,
    dvVaultName,

    -- * Destructuring the response
    DescribeVaultOutput (..),
    mkDescribeVaultOutput,

    -- ** Response lenses
    dvoVaultName,
    dvoSizeInBytes,
    dvoLastInventoryDate,
    dvoVaultARN,
    dvoCreationDate,
    dvoNumberOfArchives,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving metadata for a specific vault in Amazon Glacier.
--
-- /See:/ 'mkDescribeVault' smart constructor.
data DescribeVault = DescribeVault'
  { accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVault' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkDescribeVault ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  DescribeVault
mkDescribeVault pAccountId_ pVaultName_ =
  DescribeVault' {accountId = pAccountId_, vaultName = pVaultName_}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvAccountId :: Lens.Lens' DescribeVault Lude.Text
dvAccountId = Lens.lens (accountId :: DescribeVault -> Lude.Text) (\s a -> s {accountId = a} :: DescribeVault)
{-# DEPRECATED dvAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVaultName :: Lens.Lens' DescribeVault Lude.Text
dvVaultName = Lens.lens (vaultName :: DescribeVault -> Lude.Text) (\s a -> s {vaultName = a} :: DescribeVault)
{-# DEPRECATED dvVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest DescribeVault where
  type Rs DescribeVault = DescribeVaultOutput
  request = Req.get glacierService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeVault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVault where
  toPath DescribeVault' {..} =
    Lude.mconcat
      ["/", Lude.toBS accountId, "/vaults/", Lude.toBS vaultName]

instance Lude.ToQuery DescribeVault where
  toQuery = Lude.const Lude.mempty

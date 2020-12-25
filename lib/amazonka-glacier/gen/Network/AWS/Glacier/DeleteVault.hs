{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a vault. Amazon S3 Glacier will delete a vault only if there are no archives in the vault as of the last inventory and there have been no writes to the vault since the last inventory. If either of these conditions is not satisfied, the vault deletion fails (that is, the vault is not removed) and Amazon S3 Glacier returns an error. You can use 'DescribeVault' to return the number of archives in a vault, and you can use <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job (POST jobs)> to initiate a new inventory retrieval for a vault. The inventory contains the archive IDs you use to delete archives using <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive (DELETE archive)> .
--
-- This operation is idempotent.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html Deleting a Vault in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html Delete Vault > in the /Amazon S3 Glacier Developer Guide/ .
module Network.AWS.Glacier.DeleteVault
  ( -- * Creating a request
    DeleteVault (..),
    mkDeleteVault,

    -- ** Request lenses
    dAccountId,
    dVaultName,

    -- * Destructuring the response
    DeleteVaultResponse (..),
    mkDeleteVaultResponse,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting a vault from Amazon S3 Glacier.
--
-- /See:/ 'mkDeleteVault' smart constructor.
data DeleteVault = DeleteVault'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVault' value with any optional fields omitted.
mkDeleteVault ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  DeleteVault
mkDeleteVault accountId vaultName =
  DeleteVault' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccountId :: Lens.Lens' DeleteVault Types.String
dAccountId = Lens.field @"accountId"
{-# DEPRECATED dAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVaultName :: Lens.Lens' DeleteVault Types.String
dVaultName = Lens.field @"vaultName"
{-# DEPRECATED dVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.AWSRequest DeleteVault where
  type Rs DeleteVault = DeleteVaultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteVaultResponse'

-- | /See:/ 'mkDeleteVaultResponse' smart constructor.
data DeleteVaultResponse = DeleteVaultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVaultResponse' value with any optional fields omitted.
mkDeleteVaultResponse ::
  DeleteVaultResponse
mkDeleteVaultResponse = DeleteVaultResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes an archive from a vault. Subsequent requests to initiate a retrieval of this archive will fail. Archive retrievals that are in progress for this archive ID may or may not succeed according to the following scenarios:
--
--
--     * If the archive retrieval job is actively preparing the data for download when Amazon S3 Glacier receives the delete archive request, the archival retrieval operation might fail.
--
--
--     * If the archive retrieval job has successfully prepared the archive for download when Amazon S3 Glacier receives the delete archive request, you will be able to download the output.
--
--
-- This operation is idempotent. Attempting to delete an already-deleted archive does not result in an error.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html Deleting an Archive in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.DeleteArchive
  ( -- * Creating a request
    DeleteArchive (..),
    mkDeleteArchive,

    -- ** Request lenses
    daAccountId,
    daVaultName,
    daArchiveId,

    -- * Destructuring the response
    DeleteArchiveResponse (..),
    mkDeleteArchiveResponse,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting an archive from an Amazon S3 Glacier vault.
--
-- /See:/ 'mkDeleteArchive' smart constructor.
data DeleteArchive = DeleteArchive'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | The ID of the archive to delete.
    archiveId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchive' value with any optional fields omitted.
mkDeleteArchive ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  -- | 'archiveId'
  Types.String ->
  DeleteArchive
mkDeleteArchive accountId vaultName archiveId =
  DeleteArchive' {accountId, vaultName, archiveId}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAccountId :: Lens.Lens' DeleteArchive Types.String
daAccountId = Lens.field @"accountId"
{-# DEPRECATED daAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daVaultName :: Lens.Lens' DeleteArchive Types.String
daVaultName = Lens.field @"vaultName"
{-# DEPRECATED daVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The ID of the archive to delete.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArchiveId :: Lens.Lens' DeleteArchive Types.String
daArchiveId = Lens.field @"archiveId"
{-# DEPRECATED daArchiveId "Use generic-lens or generic-optics with 'archiveId' instead." #-}

instance Core.AWSRequest DeleteArchive where
  type Rs DeleteArchive = DeleteArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/archives/")
                Core.<> (Core.toText archiveId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteArchiveResponse'

-- | /See:/ 'mkDeleteArchiveResponse' smart constructor.
data DeleteArchiveResponse = DeleteArchiveResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchiveResponse' value with any optional fields omitted.
mkDeleteArchiveResponse ::
  DeleteArchiveResponse
mkDeleteArchiveResponse = DeleteArchiveResponse'

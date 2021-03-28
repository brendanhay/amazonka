{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.UploadArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds an archive to a vault. This is a synchronous operation, and for a successful upload, your data is durably persisted. Amazon S3 Glacier returns the archive ID in the @x-amz-archive-id@ header of the response. 
--
-- You must use the archive ID to access your data in Amazon S3 Glacier. After you upload an archive, you should save the archive ID returned so that you can retrieve or delete the archive later. Besides saving the archive ID, you can also index it and give it a friendly name to allow for better searching. You can also use the optional archive description field to specify how the archive is referred to in an external index of archives, such as you might create in Amazon DynamoDB. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see 'InitiateJob' . 
-- You must provide a SHA256 tree hash of the data you are uploading. For information about computing a SHA256 tree hash, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums> . 
-- You can optionally specify an archive description of up to 1,024 printable ASCII characters. You can get the archive description when you either retrieve the archive or get the vault inventory. For more information, see 'InitiateJob' . Amazon Glacier does not interpret the description in any way. An archive description does not need to be unique. You cannot use the description to retrieve or sort the archive list. 
-- Archives are immutable. After you upload an archive, you cannot edit the archive or its description.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html Uploading an Archive in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> in the /Amazon Glacier Developer Guide/ . 
module Network.AWS.Glacier.UploadArchive
    (
    -- * Creating a request
      UploadArchive (..)
    , mkUploadArchive
    -- ** Request lenses
    , uaVaultName
    , uaAccountId
    , uaArchiveDescription
    , uaBody
    , uaChecksum

     -- * Destructuring the response
    , Types.ArchiveCreationOutput (..)
    , Types.mkArchiveCreationOutput
    -- ** Response lenses
    , Types.acoArchiveId
    , Types.acoChecksum
    , Types.acoLocation
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to add an archive to a vault.
--
-- /See:/ 'mkUploadArchive' smart constructor.
data UploadArchive = UploadArchive'
  { vaultName :: Core.Text
    -- ^ The name of the vault.
  , accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , archiveDescription :: Core.Maybe Core.Text
    -- ^ The optional description of the archive you are uploading.
  , body :: Core.HashedBody
    -- ^ The data to upload.
  , checksum :: Core.Maybe Core.Text
    -- ^ The SHA256 tree hash of the data being uploaded.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'UploadArchive' value with any optional fields omitted.
mkUploadArchive
    :: Core.Text -- ^ 'vaultName'
    -> Core.Text -- ^ 'accountId'
    -> Core.HashedBody -- ^ 'body'
    -> UploadArchive
mkUploadArchive vaultName accountId body
  = UploadArchive'{vaultName, accountId,
                   archiveDescription = Core.Nothing, body, checksum = Core.Nothing}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaVaultName :: Lens.Lens' UploadArchive Core.Text
uaVaultName = Lens.field @"vaultName"
{-# INLINEABLE uaVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAccountId :: Lens.Lens' UploadArchive Core.Text
uaAccountId = Lens.field @"accountId"
{-# INLINEABLE uaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The optional description of the archive you are uploading.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaArchiveDescription :: Lens.Lens' UploadArchive (Core.Maybe Core.Text)
uaArchiveDescription = Lens.field @"archiveDescription"
{-# INLINEABLE uaArchiveDescription #-}
{-# DEPRECATED archiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead"  #-}

-- | The data to upload.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaBody :: Lens.Lens' UploadArchive Core.HashedBody
uaBody = Lens.field @"body"
{-# INLINEABLE uaBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The SHA256 tree hash of the data being uploaded.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaChecksum :: Lens.Lens' UploadArchive (Core.Maybe Core.Text)
uaChecksum = Lens.field @"checksum"
{-# INLINEABLE uaChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

instance Core.ToQuery UploadArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UploadArchive where
        toHeaders UploadArchive{..}
          = Core.toHeaders "x-amz-archive-description" archiveDescription
              Core.<> Core.toHeaders "x-amz-sha256-tree-hash" checksum

instance Core.AWSRequest UploadArchive where
        type Rs UploadArchive = Types.ArchiveCreationOutput
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/archives",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ArchiveCreationOutput' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-archive-id" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-sha256-tree-hash" h
                     Core.<*> Core.parseHeaderMaybe "Location" h)
        
        {-# INLINE parseResponse #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a disk into an EBS snapshot.
module Network.AWS.EC2.ImportSnapshot
  ( -- * Creating a request
    ImportSnapshot (..),
    mkImportSnapshot,

    -- ** Request lenses
    isClientData,
    isClientToken,
    isDescription,
    isDiskContainer,
    isDryRun,
    isEncrypted,
    isKmsKeyId,
    isRoleName,
    isTagSpecifications,

    -- * Destructuring the response
    ImportSnapshotResponse (..),
    mkImportSnapshotResponse,

    -- ** Response lenses
    isrrsDescription,
    isrrsImportTaskId,
    isrrsSnapshotTaskDetail,
    isrrsTags,
    isrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { -- | The client-specific data.
    clientData :: Core.Maybe Types.ClientData,
    -- | Token to enable idempotency for VM import requests.
    clientToken :: Core.Maybe Types.String,
    -- | The description string for the import snapshot task.
    description :: Core.Maybe Types.String,
    -- | Information about the disk container.
    diskContainer :: Core.Maybe Types.SnapshotDiskContainer,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Core.Maybe Core.Bool,
    -- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
    --
    -- The CMK identifier may be provided in any of the following formats:
    --
    --     * Key ID
    --
    --
    --     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
    --
    --
    --     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
    --
    --
    --     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
    --
    --
    -- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.
    -- The specified CMK must exist in the Region that the snapshot is being copied to.
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of the role to use when not using the default role, 'vmimport'.
    roleName :: Core.Maybe Types.String,
    -- | The tags to apply to the snapshot being imported.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImportSnapshot' value with any optional fields omitted.
mkImportSnapshot ::
  ImportSnapshot
mkImportSnapshot =
  ImportSnapshot'
    { clientData = Core.Nothing,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      diskContainer = Core.Nothing,
      dryRun = Core.Nothing,
      encrypted = Core.Nothing,
      kmsKeyId = Core.Nothing,
      roleName = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientData :: Lens.Lens' ImportSnapshot (Core.Maybe Types.ClientData)
isClientData = Lens.field @"clientData"
{-# DEPRECATED isClientData "Use generic-lens or generic-optics with 'clientData' instead." #-}

-- | Token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientToken :: Lens.Lens' ImportSnapshot (Core.Maybe Types.String)
isClientToken = Lens.field @"clientToken"
{-# DEPRECATED isClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The description string for the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDescription :: Lens.Lens' ImportSnapshot (Core.Maybe Types.String)
isDescription = Lens.field @"description"
{-# DEPRECATED isDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the disk container.
--
-- /Note:/ Consider using 'diskContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDiskContainer :: Lens.Lens' ImportSnapshot (Core.Maybe Types.SnapshotDiskContainer)
isDiskContainer = Lens.field @"diskContainer"
{-# DEPRECATED isDiskContainer "Use generic-lens or generic-optics with 'diskContainer' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDryRun :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
isDryRun = Lens.field @"dryRun"
{-# DEPRECATED isDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isEncrypted :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
isEncrypted = Lens.field @"encrypted"
{-# DEPRECATED isEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
--     * Key ID
--
--
--     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
--     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
--
--
--     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.
-- The specified CMK must exist in the Region that the snapshot is being copied to.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isKmsKeyId :: Lens.Lens' ImportSnapshot (Core.Maybe Types.KmsKeyId)
isKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED isKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isRoleName :: Lens.Lens' ImportSnapshot (Core.Maybe Types.String)
isRoleName = Lens.field @"roleName"
{-# DEPRECATED isRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The tags to apply to the snapshot being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTagSpecifications :: Lens.Lens' ImportSnapshot (Core.Maybe [Types.TagSpecification])
isTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED isTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest ImportSnapshot where
  type Rs ImportSnapshot = ImportSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ImportSnapshot")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "ClientData" Core.<$> clientData)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DiskContainer" Core.<$> diskContainer)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Encrypted" Core.<$> encrypted)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "RoleName" Core.<$> roleName)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ImportSnapshotResponse'
            Core.<$> (x Core..@? "description")
            Core.<*> (x Core..@? "importTaskId")
            Core.<*> (x Core..@? "snapshotTaskDetail")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { -- | A description of the import snapshot task.
    description :: Core.Maybe Types.String,
    -- | The ID of the import snapshot task.
    importTaskId :: Core.Maybe Types.String,
    -- | Information about the import snapshot task.
    snapshotTaskDetail :: Core.Maybe Types.SnapshotTaskDetail,
    -- | Any tags assigned to the snapshot being imported.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportSnapshotResponse' value with any optional fields omitted.
mkImportSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportSnapshotResponse
mkImportSnapshotResponse responseStatus =
  ImportSnapshotResponse'
    { description = Core.Nothing,
      importTaskId = Core.Nothing,
      snapshotTaskDetail = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | A description of the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsDescription :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Types.String)
isrrsDescription = Lens.field @"description"
{-# DEPRECATED isrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the import snapshot task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsImportTaskId :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Types.String)
isrrsImportTaskId = Lens.field @"importTaskId"
{-# DEPRECATED isrrsImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | Information about the import snapshot task.
--
-- /Note:/ Consider using 'snapshotTaskDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsSnapshotTaskDetail :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Types.SnapshotTaskDetail)
isrrsSnapshotTaskDetail = Lens.field @"snapshotTaskDetail"
{-# DEPRECATED isrrsSnapshotTaskDetail "Use generic-lens or generic-optics with 'snapshotTaskDetail' instead." #-}

-- | Any tags assigned to the snapshot being imported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsTags :: Lens.Lens' ImportSnapshotResponse (Core.Maybe [Types.Tag])
isrrsTags = Lens.field @"tags"
{-# DEPRECATED isrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsResponseStatus :: Lens.Lens' ImportSnapshotResponse Core.Int
isrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED isrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

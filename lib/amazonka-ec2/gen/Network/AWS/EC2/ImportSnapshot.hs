{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ImportSnapshot (..)
    , mkImportSnapshot
    -- ** Request lenses
    , isClientData
    , isClientToken
    , isDescription
    , isDiskContainer
    , isDryRun
    , isEncrypted
    , isKmsKeyId
    , isRoleName
    , isTagSpecifications

    -- * Destructuring the response
    , ImportSnapshotResponse (..)
    , mkImportSnapshotResponse
    -- ** Response lenses
    , isrrsDescription
    , isrrsImportTaskId
    , isrrsSnapshotTaskDetail
    , isrrsTags
    , isrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { clientData :: Core.Maybe Types.ClientData
    -- ^ The client-specific data.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Token to enable idempotency for VM import requests.
  , description :: Core.Maybe Core.Text
    -- ^ The description string for the import snapshot task.
  , diskContainer :: Core.Maybe Types.SnapshotDiskContainer
    -- ^ Information about the disk container.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set. 
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
  , roleName :: Core.Maybe Core.Text
    -- ^ The name of the role to use when not using the default role, 'vmimport'.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the snapshot being imported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportSnapshot' value with any optional fields omitted.
mkImportSnapshot
    :: ImportSnapshot
mkImportSnapshot
  = ImportSnapshot'{clientData = Core.Nothing,
                    clientToken = Core.Nothing, description = Core.Nothing,
                    diskContainer = Core.Nothing, dryRun = Core.Nothing,
                    encrypted = Core.Nothing, kmsKeyId = Core.Nothing,
                    roleName = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientData :: Lens.Lens' ImportSnapshot (Core.Maybe Types.ClientData)
isClientData = Lens.field @"clientData"
{-# INLINEABLE isClientData #-}
{-# DEPRECATED clientData "Use generic-lens or generic-optics with 'clientData' instead"  #-}

-- | Token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientToken :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
isClientToken = Lens.field @"clientToken"
{-# INLINEABLE isClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description string for the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDescription :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
isDescription = Lens.field @"description"
{-# INLINEABLE isDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the disk container.
--
-- /Note:/ Consider using 'diskContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDiskContainer :: Lens.Lens' ImportSnapshot (Core.Maybe Types.SnapshotDiskContainer)
isDiskContainer = Lens.field @"diskContainer"
{-# INLINEABLE isDiskContainer #-}
{-# DEPRECATED diskContainer "Use generic-lens or generic-optics with 'diskContainer' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDryRun :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
isDryRun = Lens.field @"dryRun"
{-# INLINEABLE isDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isEncrypted :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
isEncrypted = Lens.field @"encrypted"
{-# INLINEABLE isEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

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
{-# INLINEABLE isKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isRoleName :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
isRoleName = Lens.field @"roleName"
{-# INLINEABLE isRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The tags to apply to the snapshot being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTagSpecifications :: Lens.Lens' ImportSnapshot (Core.Maybe [Types.TagSpecification])
isTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE isTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery ImportSnapshot where
        toQuery ImportSnapshot{..}
          = Core.toQueryPair "Action" ("ImportSnapshot" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientData") clientData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DiskContainer")
                diskContainer
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RoleName") roleName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders ImportSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ImportSnapshot where
        type Rs ImportSnapshot = ImportSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ImportSnapshotResponse' Core.<$>
                   (x Core..@? "description") Core.<*> x Core..@? "importTaskId"
                     Core.<*> x Core..@? "snapshotTaskDetail"
                     Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the import snapshot task.
  , importTaskId :: Core.Maybe Core.Text
    -- ^ The ID of the import snapshot task.
  , snapshotTaskDetail :: Core.Maybe Types.SnapshotTaskDetail
    -- ^ Information about the import snapshot task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the snapshot being imported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportSnapshotResponse' value with any optional fields omitted.
mkImportSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportSnapshotResponse
mkImportSnapshotResponse responseStatus
  = ImportSnapshotResponse'{description = Core.Nothing,
                            importTaskId = Core.Nothing, snapshotTaskDetail = Core.Nothing,
                            tags = Core.Nothing, responseStatus}

-- | A description of the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsDescription :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Core.Text)
isrrsDescription = Lens.field @"description"
{-# INLINEABLE isrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the import snapshot task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsImportTaskId :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Core.Text)
isrrsImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE isrrsImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | Information about the import snapshot task.
--
-- /Note:/ Consider using 'snapshotTaskDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsSnapshotTaskDetail :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Types.SnapshotTaskDetail)
isrrsSnapshotTaskDetail = Lens.field @"snapshotTaskDetail"
{-# INLINEABLE isrrsSnapshotTaskDetail #-}
{-# DEPRECATED snapshotTaskDetail "Use generic-lens or generic-optics with 'snapshotTaskDetail' instead"  #-}

-- | Any tags assigned to the snapshot being imported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsTags :: Lens.Lens' ImportSnapshotResponse (Core.Maybe [Types.Tag])
isrrsTags = Lens.field @"tags"
{-# INLINEABLE isrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrrsResponseStatus :: Lens.Lens' ImportSnapshotResponse Core.Int
isrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE isrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

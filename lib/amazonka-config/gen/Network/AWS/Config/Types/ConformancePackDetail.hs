{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackDetail
  ( ConformancePackDetail (..),

    -- * Smart constructor
    mkConformancePackDetail,

    -- * Lenses
    cpdConformancePackName,
    cpdConformancePackArn,
    cpdConformancePackId,
    cpdConformancePackInputParameters,
    cpdCreatedBy,
    cpdDeliveryS3Bucket,
    cpdDeliveryS3KeyPrefix,
    cpdLastUpdateRequestedTime,
  )
where

import qualified Network.AWS.Config.Types.ConformancePackArn as Types
import qualified Network.AWS.Config.Types.ConformancePackId as Types
import qualified Network.AWS.Config.Types.ConformancePackInputParameter as Types
import qualified Network.AWS.Config.Types.ConformancePackName as Types
import qualified Network.AWS.Config.Types.CreatedBy as Types
import qualified Network.AWS.Config.Types.DeliveryS3Bucket as Types
import qualified Network.AWS.Config.Types.DeliveryS3KeyPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns details of a conformance pack. A conformance pack is a collection of AWS Config rules and remediation actions that can be easily deployed in an account and a region.
--
-- /See:/ 'mkConformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { -- | Name of the conformance pack.
    conformancePackName :: Types.ConformancePackName,
    -- | Amazon Resource Name (ARN) of the conformance pack.
    conformancePackArn :: Types.ConformancePackArn,
    -- | ID of the conformance pack.
    conformancePackId :: Types.ConformancePackId,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter],
    -- | AWS service that created the conformance pack.
    createdBy :: Core.Maybe Types.CreatedBy,
    -- | Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
    deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket,
    -- | The prefix for the Amazon S3 bucket.
    deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix,
    -- | Last time when conformation pack update was requested.
    lastUpdateRequestedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ConformancePackDetail' value with any optional fields omitted.
mkConformancePackDetail ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  -- | 'conformancePackArn'
  Types.ConformancePackArn ->
  -- | 'conformancePackId'
  Types.ConformancePackId ->
  ConformancePackDetail
mkConformancePackDetail
  conformancePackName
  conformancePackArn
  conformancePackId =
    ConformancePackDetail'
      { conformancePackName,
        conformancePackArn,
        conformancePackId,
        conformancePackInputParameters = Core.Nothing,
        createdBy = Core.Nothing,
        deliveryS3Bucket = Core.Nothing,
        deliveryS3KeyPrefix = Core.Nothing,
        lastUpdateRequestedTime = Core.Nothing
      }

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackName :: Lens.Lens' ConformancePackDetail Types.ConformancePackName
cpdConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED cpdConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | Amazon Resource Name (ARN) of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackArn :: Lens.Lens' ConformancePackDetail Types.ConformancePackArn
cpdConformancePackArn = Lens.field @"conformancePackArn"
{-# DEPRECATED cpdConformancePackArn "Use generic-lens or generic-optics with 'conformancePackArn' instead." #-}

-- | ID of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackId :: Lens.Lens' ConformancePackDetail Types.ConformancePackId
cpdConformancePackId = Lens.field @"conformancePackId"
{-# DEPRECATED cpdConformancePackId "Use generic-lens or generic-optics with 'conformancePackId' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackInputParameters :: Lens.Lens' ConformancePackDetail (Core.Maybe [Types.ConformancePackInputParameter])
cpdConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# DEPRECATED cpdConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | AWS service that created the conformance pack.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdCreatedBy :: Lens.Lens' ConformancePackDetail (Core.Maybe Types.CreatedBy)
cpdCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED cpdCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDeliveryS3Bucket :: Lens.Lens' ConformancePackDetail (Core.Maybe Types.DeliveryS3Bucket)
cpdDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# DEPRECATED cpdDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDeliveryS3KeyPrefix :: Lens.Lens' ConformancePackDetail (Core.Maybe Types.DeliveryS3KeyPrefix)
cpdDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# DEPRECATED cpdDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | Last time when conformation pack update was requested.
--
-- /Note:/ Consider using 'lastUpdateRequestedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdLastUpdateRequestedTime :: Lens.Lens' ConformancePackDetail (Core.Maybe Core.NominalDiffTime)
cpdLastUpdateRequestedTime = Lens.field @"lastUpdateRequestedTime"
{-# DEPRECATED cpdLastUpdateRequestedTime "Use generic-lens or generic-optics with 'lastUpdateRequestedTime' instead." #-}

instance Core.FromJSON ConformancePackDetail where
  parseJSON =
    Core.withObject "ConformancePackDetail" Core.$
      \x ->
        ConformancePackDetail'
          Core.<$> (x Core..: "ConformancePackName")
          Core.<*> (x Core..: "ConformancePackArn")
          Core.<*> (x Core..: "ConformancePackId")
          Core.<*> (x Core..:? "ConformancePackInputParameters")
          Core.<*> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "DeliveryS3Bucket")
          Core.<*> (x Core..:? "DeliveryS3KeyPrefix")
          Core.<*> (x Core..:? "LastUpdateRequestedTime")

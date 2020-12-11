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
    cpdDeliveryS3Bucket,
    cpdDeliveryS3KeyPrefix,
    cpdCreatedBy,
    cpdLastUpdateRequestedTime,
    cpdConformancePackInputParameters,
    cpdConformancePackName,
    cpdConformancePackARN,
    cpdConformancePackId,
  )
where

import Network.AWS.Config.Types.ConformancePackInputParameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns details of a conformance pack. A conformance pack is a collection of AWS Config rules and remediation actions that can be easily deployed in an account and a region.
--
-- /See:/ 'mkConformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { deliveryS3Bucket ::
      Lude.Maybe Lude.Text,
    deliveryS3KeyPrefix :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    lastUpdateRequestedTime ::
      Lude.Maybe Lude.Timestamp,
    conformancePackInputParameters ::
      Lude.Maybe [ConformancePackInputParameter],
    conformancePackName :: Lude.Text,
    conformancePackARN :: Lude.Text,
    conformancePackId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackDetail' with the minimum fields required to make a request.
--
-- * 'conformancePackARN' - Amazon Resource Name (ARN) of the conformance pack.
-- * 'conformancePackId' - ID of the conformance pack.
-- * 'conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'createdBy' - AWS service that created the conformance pack.
-- * 'deliveryS3Bucket' - Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
-- * 'deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
-- * 'lastUpdateRequestedTime' - Last time when conformation pack update was requested.
mkConformancePackDetail ::
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'conformancePackARN'
  Lude.Text ->
  -- | 'conformancePackId'
  Lude.Text ->
  ConformancePackDetail
mkConformancePackDetail
  pConformancePackName_
  pConformancePackARN_
  pConformancePackId_ =
    ConformancePackDetail'
      { deliveryS3Bucket = Lude.Nothing,
        deliveryS3KeyPrefix = Lude.Nothing,
        createdBy = Lude.Nothing,
        lastUpdateRequestedTime = Lude.Nothing,
        conformancePackInputParameters = Lude.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackARN = pConformancePackARN_,
        conformancePackId = pConformancePackId_
      }

-- | Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDeliveryS3Bucket :: Lens.Lens' ConformancePackDetail (Lude.Maybe Lude.Text)
cpdDeliveryS3Bucket = Lens.lens (deliveryS3Bucket :: ConformancePackDetail -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3Bucket = a} :: ConformancePackDetail)
{-# DEPRECATED cpdDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDeliveryS3KeyPrefix :: Lens.Lens' ConformancePackDetail (Lude.Maybe Lude.Text)
cpdDeliveryS3KeyPrefix = Lens.lens (deliveryS3KeyPrefix :: ConformancePackDetail -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3KeyPrefix = a} :: ConformancePackDetail)
{-# DEPRECATED cpdDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | AWS service that created the conformance pack.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdCreatedBy :: Lens.Lens' ConformancePackDetail (Lude.Maybe Lude.Text)
cpdCreatedBy = Lens.lens (createdBy :: ConformancePackDetail -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: ConformancePackDetail)
{-# DEPRECATED cpdCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | Last time when conformation pack update was requested.
--
-- /Note:/ Consider using 'lastUpdateRequestedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdLastUpdateRequestedTime :: Lens.Lens' ConformancePackDetail (Lude.Maybe Lude.Timestamp)
cpdLastUpdateRequestedTime = Lens.lens (lastUpdateRequestedTime :: ConformancePackDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateRequestedTime = a} :: ConformancePackDetail)
{-# DEPRECATED cpdLastUpdateRequestedTime "Use generic-lens or generic-optics with 'lastUpdateRequestedTime' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackInputParameters :: Lens.Lens' ConformancePackDetail (Lude.Maybe [ConformancePackInputParameter])
cpdConformancePackInputParameters = Lens.lens (conformancePackInputParameters :: ConformancePackDetail -> Lude.Maybe [ConformancePackInputParameter]) (\s a -> s {conformancePackInputParameters = a} :: ConformancePackDetail)
{-# DEPRECATED cpdConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackName :: Lens.Lens' ConformancePackDetail Lude.Text
cpdConformancePackName = Lens.lens (conformancePackName :: ConformancePackDetail -> Lude.Text) (\s a -> s {conformancePackName = a} :: ConformancePackDetail)
{-# DEPRECATED cpdConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | Amazon Resource Name (ARN) of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackARN :: Lens.Lens' ConformancePackDetail Lude.Text
cpdConformancePackARN = Lens.lens (conformancePackARN :: ConformancePackDetail -> Lude.Text) (\s a -> s {conformancePackARN = a} :: ConformancePackDetail)
{-# DEPRECATED cpdConformancePackARN "Use generic-lens or generic-optics with 'conformancePackARN' instead." #-}

-- | ID of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdConformancePackId :: Lens.Lens' ConformancePackDetail Lude.Text
cpdConformancePackId = Lens.lens (conformancePackId :: ConformancePackDetail -> Lude.Text) (\s a -> s {conformancePackId = a} :: ConformancePackDetail)
{-# DEPRECATED cpdConformancePackId "Use generic-lens or generic-optics with 'conformancePackId' instead." #-}

instance Lude.FromJSON ConformancePackDetail where
  parseJSON =
    Lude.withObject
      "ConformancePackDetail"
      ( \x ->
          ConformancePackDetail'
            Lude.<$> (x Lude..:? "DeliveryS3Bucket")
            Lude.<*> (x Lude..:? "DeliveryS3KeyPrefix")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastUpdateRequestedTime")
            Lude.<*> (x Lude..:? "ConformancePackInputParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ConformancePackName")
            Lude.<*> (x Lude..: "ConformancePackArn")
            Lude.<*> (x Lude..: "ConformancePackId")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamDescription
  ( DeliveryStreamDescription (..),

    -- * Smart constructor
    mkDeliveryStreamDescription,

    -- * Lenses
    dsdDeliveryStreamStatus,
    dsdVersionId,
    dsdDeliveryStreamARN,
    dsdHasMoreDestinations,
    dsdFailureDescription,
    dsdDeliveryStreamEncryptionConfiguration,
    dsdDestinations,
    dsdDeliveryStreamName,
    dsdCreateTimestamp,
    dsdSource,
    dsdLastUpdateTimestamp,
    dsdDeliveryStreamType,
  )
where

import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamStatus
import Network.AWS.Firehose.Types.DeliveryStreamType
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.SourceDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a delivery stream.
--
-- /See:/ 'mkDeliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { -- | The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
    deliveryStreamStatus :: DeliveryStreamStatus,
    -- | Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
    versionId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    deliveryStreamARN :: Lude.Text,
    -- | Indicates whether there are more destinations available to list.
    hasMoreDestinations :: Lude.Bool,
    -- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
    failureDescription :: Lude.Maybe FailureDescription,
    -- | Indicates the server-side encryption (SSE) status for the delivery stream.
    deliveryStreamEncryptionConfiguration :: Lude.Maybe DeliveryStreamEncryptionConfiguration,
    -- | The destinations.
    destinations :: [DestinationDescription],
    -- | The name of the delivery stream.
    deliveryStreamName :: Lude.Text,
    -- | The date and time that the delivery stream was created.
    createTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
    source :: Lude.Maybe SourceDescription,
    -- | The date and time that the delivery stream was last updated.
    lastUpdateTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The delivery stream type. This can be one of the following values:
    --
    --
    --     * @DirectPut@ : Provider applications access the delivery stream directly.
    --
    --
    --     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
    deliveryStreamType :: DeliveryStreamType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- * 'deliveryStreamStatus' - The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
-- * 'versionId' - Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
-- * 'deliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'hasMoreDestinations' - Indicates whether there are more destinations available to list.
-- * 'failureDescription' - Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
-- * 'deliveryStreamEncryptionConfiguration' - Indicates the server-side encryption (SSE) status for the delivery stream.
-- * 'destinations' - The destinations.
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'createTimestamp' - The date and time that the delivery stream was created.
-- * 'source' - If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
-- * 'lastUpdateTimestamp' - The date and time that the delivery stream was last updated.
-- * 'deliveryStreamType' - The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
mkDeliveryStreamDescription ::
  -- | 'deliveryStreamStatus'
  DeliveryStreamStatus ->
  -- | 'versionId'
  Lude.Text ->
  -- | 'deliveryStreamARN'
  Lude.Text ->
  -- | 'hasMoreDestinations'
  Lude.Bool ->
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'deliveryStreamType'
  DeliveryStreamType ->
  DeliveryStreamDescription
mkDeliveryStreamDescription
  pDeliveryStreamStatus_
  pVersionId_
  pDeliveryStreamARN_
  pHasMoreDestinations_
  pDeliveryStreamName_
  pDeliveryStreamType_ =
    DeliveryStreamDescription'
      { deliveryStreamStatus =
          pDeliveryStreamStatus_,
        versionId = pVersionId_,
        deliveryStreamARN = pDeliveryStreamARN_,
        hasMoreDestinations = pHasMoreDestinations_,
        failureDescription = Lude.Nothing,
        deliveryStreamEncryptionConfiguration = Lude.Nothing,
        destinations = Lude.mempty,
        deliveryStreamName = pDeliveryStreamName_,
        createTimestamp = Lude.Nothing,
        source = Lude.Nothing,
        lastUpdateTimestamp = Lude.Nothing,
        deliveryStreamType = pDeliveryStreamType_
      }

-- | The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
--
-- /Note:/ Consider using 'deliveryStreamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamStatus :: Lens.Lens' DeliveryStreamDescription DeliveryStreamStatus
dsdDeliveryStreamStatus = Lens.lens (deliveryStreamStatus :: DeliveryStreamDescription -> DeliveryStreamStatus) (\s a -> s {deliveryStreamStatus = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDeliveryStreamStatus "Use generic-lens or generic-optics with 'deliveryStreamStatus' instead." #-}

-- | Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdVersionId :: Lens.Lens' DeliveryStreamDescription Lude.Text
dsdVersionId = Lens.lens (versionId :: DeliveryStreamDescription -> Lude.Text) (\s a -> s {versionId = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamARN :: Lens.Lens' DeliveryStreamDescription Lude.Text
dsdDeliveryStreamARN = Lens.lens (deliveryStreamARN :: DeliveryStreamDescription -> Lude.Text) (\s a -> s {deliveryStreamARN = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDeliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead." #-}

-- | Indicates whether there are more destinations available to list.
--
-- /Note:/ Consider using 'hasMoreDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdHasMoreDestinations :: Lens.Lens' DeliveryStreamDescription Lude.Bool
dsdHasMoreDestinations = Lens.lens (hasMoreDestinations :: DeliveryStreamDescription -> Lude.Bool) (\s a -> s {hasMoreDestinations = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdHasMoreDestinations "Use generic-lens or generic-optics with 'hasMoreDestinations' instead." #-}

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdFailureDescription :: Lens.Lens' DeliveryStreamDescription (Lude.Maybe FailureDescription)
dsdFailureDescription = Lens.lens (failureDescription :: DeliveryStreamDescription -> Lude.Maybe FailureDescription) (\s a -> s {failureDescription = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | Indicates the server-side encryption (SSE) status for the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamEncryptionConfiguration :: Lens.Lens' DeliveryStreamDescription (Lude.Maybe DeliveryStreamEncryptionConfiguration)
dsdDeliveryStreamEncryptionConfiguration = Lens.lens (deliveryStreamEncryptionConfiguration :: DeliveryStreamDescription -> Lude.Maybe DeliveryStreamEncryptionConfiguration) (\s a -> s {deliveryStreamEncryptionConfiguration = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDeliveryStreamEncryptionConfiguration "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfiguration' instead." #-}

-- | The destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDestinations :: Lens.Lens' DeliveryStreamDescription [DestinationDescription]
dsdDestinations = Lens.lens (destinations :: DeliveryStreamDescription -> [DestinationDescription]) (\s a -> s {destinations = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamName :: Lens.Lens' DeliveryStreamDescription Lude.Text
dsdDeliveryStreamName = Lens.lens (deliveryStreamName :: DeliveryStreamDescription -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The date and time that the delivery stream was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdCreateTimestamp :: Lens.Lens' DeliveryStreamDescription (Lude.Maybe Lude.Timestamp)
dsdCreateTimestamp = Lens.lens (createTimestamp :: DeliveryStreamDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTimestamp = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSource :: Lens.Lens' DeliveryStreamDescription (Lude.Maybe SourceDescription)
dsdSource = Lens.lens (source :: DeliveryStreamDescription -> Lude.Maybe SourceDescription) (\s a -> s {source = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The date and time that the delivery stream was last updated.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLastUpdateTimestamp :: Lens.Lens' DeliveryStreamDescription (Lude.Maybe Lude.Timestamp)
dsdLastUpdateTimestamp = Lens.lens (lastUpdateTimestamp :: DeliveryStreamDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTimestamp = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdLastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead." #-}

-- | The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
--
-- /Note:/ Consider using 'deliveryStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamType :: Lens.Lens' DeliveryStreamDescription DeliveryStreamType
dsdDeliveryStreamType = Lens.lens (deliveryStreamType :: DeliveryStreamDescription -> DeliveryStreamType) (\s a -> s {deliveryStreamType = a} :: DeliveryStreamDescription)
{-# DEPRECATED dsdDeliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead." #-}

instance Lude.FromJSON DeliveryStreamDescription where
  parseJSON =
    Lude.withObject
      "DeliveryStreamDescription"
      ( \x ->
          DeliveryStreamDescription'
            Lude.<$> (x Lude..: "DeliveryStreamStatus")
            Lude.<*> (x Lude..: "VersionId")
            Lude.<*> (x Lude..: "DeliveryStreamARN")
            Lude.<*> (x Lude..: "HasMoreDestinations")
            Lude.<*> (x Lude..:? "FailureDescription")
            Lude.<*> (x Lude..:? "DeliveryStreamEncryptionConfiguration")
            Lude.<*> (x Lude..:? "Destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "DeliveryStreamName")
            Lude.<*> (x Lude..:? "CreateTimestamp")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "LastUpdateTimestamp")
            Lude.<*> (x Lude..: "DeliveryStreamType")
      )

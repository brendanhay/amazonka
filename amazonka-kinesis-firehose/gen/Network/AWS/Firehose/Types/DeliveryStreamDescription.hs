{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamStatus
import Network.AWS.Firehose.Types.DeliveryStreamType
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.SourceDescription
import qualified Network.AWS.Lens as Lens

-- | Contains information about a delivery stream.
--
-- /See:/ 'newDeliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { -- | Indicates the server-side encryption (SSE) status for the delivery
    -- stream.
    deliveryStreamEncryptionConfiguration :: Core.Maybe DeliveryStreamEncryptionConfiguration,
    -- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
    -- SourceDescription object describing the source Kinesis data stream.
    source :: Core.Maybe SourceDescription,
    -- | The date and time that the delivery stream was created.
    createTimestamp :: Core.Maybe Core.POSIX,
    -- | Provides details in case one of the following operations fails due to an
    -- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
    -- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
    failureDescription :: Core.Maybe FailureDescription,
    -- | The date and time that the delivery stream was last updated.
    lastUpdateTimestamp :: Core.Maybe Core.POSIX,
    -- | The name of the delivery stream.
    deliveryStreamName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the delivery stream. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    deliveryStreamARN :: Core.Text,
    -- | The status of the delivery stream. If the status of a delivery stream is
    -- @CREATING_FAILED@, this status doesn\'t change, and you can\'t invoke
    -- @CreateDeliveryStream@ again on it. However, you can invoke the
    -- DeleteDeliveryStream operation to delete it.
    deliveryStreamStatus :: DeliveryStreamStatus,
    -- | The delivery stream type. This can be one of the following values:
    --
    -- -   @DirectPut@: Provider applications access the delivery stream
    --     directly.
    --
    -- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
    --     stream as a source.
    deliveryStreamType :: DeliveryStreamType,
    -- | Each time the destination is updated for a delivery stream, the version
    -- ID is changed, and the current version ID is required when updating the
    -- destination. This is so that the service knows it is applying the
    -- changes to the correct version of the delivery stream.
    versionId :: Core.Text,
    -- | The destinations.
    destinations :: [DestinationDescription],
    -- | Indicates whether there are more destinations available to list.
    hasMoreDestinations :: Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeliveryStreamDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamEncryptionConfiguration', 'deliveryStreamDescription_deliveryStreamEncryptionConfiguration' - Indicates the server-side encryption (SSE) status for the delivery
-- stream.
--
-- 'source', 'deliveryStreamDescription_source' - If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
-- SourceDescription object describing the source Kinesis data stream.
--
-- 'createTimestamp', 'deliveryStreamDescription_createTimestamp' - The date and time that the delivery stream was created.
--
-- 'failureDescription', 'deliveryStreamDescription_failureDescription' - Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
--
-- 'lastUpdateTimestamp', 'deliveryStreamDescription_lastUpdateTimestamp' - The date and time that the delivery stream was last updated.
--
-- 'deliveryStreamName', 'deliveryStreamDescription_deliveryStreamName' - The name of the delivery stream.
--
-- 'deliveryStreamARN', 'deliveryStreamDescription_deliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'deliveryStreamStatus', 'deliveryStreamDescription_deliveryStreamStatus' - The status of the delivery stream. If the status of a delivery stream is
-- @CREATING_FAILED@, this status doesn\'t change, and you can\'t invoke
-- @CreateDeliveryStream@ again on it. However, you can invoke the
-- DeleteDeliveryStream operation to delete it.
--
-- 'deliveryStreamType', 'deliveryStreamDescription_deliveryStreamType' - The delivery stream type. This can be one of the following values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
--
-- 'versionId', 'deliveryStreamDescription_versionId' - Each time the destination is updated for a delivery stream, the version
-- ID is changed, and the current version ID is required when updating the
-- destination. This is so that the service knows it is applying the
-- changes to the correct version of the delivery stream.
--
-- 'destinations', 'deliveryStreamDescription_destinations' - The destinations.
--
-- 'hasMoreDestinations', 'deliveryStreamDescription_hasMoreDestinations' - Indicates whether there are more destinations available to list.
newDeliveryStreamDescription ::
  -- | 'deliveryStreamName'
  Core.Text ->
  -- | 'deliveryStreamARN'
  Core.Text ->
  -- | 'deliveryStreamStatus'
  DeliveryStreamStatus ->
  -- | 'deliveryStreamType'
  DeliveryStreamType ->
  -- | 'versionId'
  Core.Text ->
  -- | 'hasMoreDestinations'
  Core.Bool ->
  DeliveryStreamDescription
newDeliveryStreamDescription
  pDeliveryStreamName_
  pDeliveryStreamARN_
  pDeliveryStreamStatus_
  pDeliveryStreamType_
  pVersionId_
  pHasMoreDestinations_ =
    DeliveryStreamDescription'
      { deliveryStreamEncryptionConfiguration =
          Core.Nothing,
        source = Core.Nothing,
        createTimestamp = Core.Nothing,
        failureDescription = Core.Nothing,
        lastUpdateTimestamp = Core.Nothing,
        deliveryStreamName = pDeliveryStreamName_,
        deliveryStreamARN = pDeliveryStreamARN_,
        deliveryStreamStatus = pDeliveryStreamStatus_,
        deliveryStreamType = pDeliveryStreamType_,
        versionId = pVersionId_,
        destinations = Core.mempty,
        hasMoreDestinations = pHasMoreDestinations_
      }

-- | Indicates the server-side encryption (SSE) status for the delivery
-- stream.
deliveryStreamDescription_deliveryStreamEncryptionConfiguration :: Lens.Lens' DeliveryStreamDescription (Core.Maybe DeliveryStreamEncryptionConfiguration)
deliveryStreamDescription_deliveryStreamEncryptionConfiguration = Lens.lens (\DeliveryStreamDescription' {deliveryStreamEncryptionConfiguration} -> deliveryStreamEncryptionConfiguration) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamEncryptionConfiguration = a} :: DeliveryStreamDescription)

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
-- SourceDescription object describing the source Kinesis data stream.
deliveryStreamDescription_source :: Lens.Lens' DeliveryStreamDescription (Core.Maybe SourceDescription)
deliveryStreamDescription_source = Lens.lens (\DeliveryStreamDescription' {source} -> source) (\s@DeliveryStreamDescription' {} a -> s {source = a} :: DeliveryStreamDescription)

-- | The date and time that the delivery stream was created.
deliveryStreamDescription_createTimestamp :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Core.UTCTime)
deliveryStreamDescription_createTimestamp = Lens.lens (\DeliveryStreamDescription' {createTimestamp} -> createTimestamp) (\s@DeliveryStreamDescription' {} a -> s {createTimestamp = a} :: DeliveryStreamDescription) Core.. Lens.mapping Core._Time

-- | Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
deliveryStreamDescription_failureDescription :: Lens.Lens' DeliveryStreamDescription (Core.Maybe FailureDescription)
deliveryStreamDescription_failureDescription = Lens.lens (\DeliveryStreamDescription' {failureDescription} -> failureDescription) (\s@DeliveryStreamDescription' {} a -> s {failureDescription = a} :: DeliveryStreamDescription)

-- | The date and time that the delivery stream was last updated.
deliveryStreamDescription_lastUpdateTimestamp :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Core.UTCTime)
deliveryStreamDescription_lastUpdateTimestamp = Lens.lens (\DeliveryStreamDescription' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@DeliveryStreamDescription' {} a -> s {lastUpdateTimestamp = a} :: DeliveryStreamDescription) Core.. Lens.mapping Core._Time

-- | The name of the delivery stream.
deliveryStreamDescription_deliveryStreamName :: Lens.Lens' DeliveryStreamDescription Core.Text
deliveryStreamDescription_deliveryStreamName = Lens.lens (\DeliveryStreamDescription' {deliveryStreamName} -> deliveryStreamName) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamName = a} :: DeliveryStreamDescription)

-- | The Amazon Resource Name (ARN) of the delivery stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
deliveryStreamDescription_deliveryStreamARN :: Lens.Lens' DeliveryStreamDescription Core.Text
deliveryStreamDescription_deliveryStreamARN = Lens.lens (\DeliveryStreamDescription' {deliveryStreamARN} -> deliveryStreamARN) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamARN = a} :: DeliveryStreamDescription)

-- | The status of the delivery stream. If the status of a delivery stream is
-- @CREATING_FAILED@, this status doesn\'t change, and you can\'t invoke
-- @CreateDeliveryStream@ again on it. However, you can invoke the
-- DeleteDeliveryStream operation to delete it.
deliveryStreamDescription_deliveryStreamStatus :: Lens.Lens' DeliveryStreamDescription DeliveryStreamStatus
deliveryStreamDescription_deliveryStreamStatus = Lens.lens (\DeliveryStreamDescription' {deliveryStreamStatus} -> deliveryStreamStatus) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamStatus = a} :: DeliveryStreamDescription)

-- | The delivery stream type. This can be one of the following values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
deliveryStreamDescription_deliveryStreamType :: Lens.Lens' DeliveryStreamDescription DeliveryStreamType
deliveryStreamDescription_deliveryStreamType = Lens.lens (\DeliveryStreamDescription' {deliveryStreamType} -> deliveryStreamType) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamType = a} :: DeliveryStreamDescription)

-- | Each time the destination is updated for a delivery stream, the version
-- ID is changed, and the current version ID is required when updating the
-- destination. This is so that the service knows it is applying the
-- changes to the correct version of the delivery stream.
deliveryStreamDescription_versionId :: Lens.Lens' DeliveryStreamDescription Core.Text
deliveryStreamDescription_versionId = Lens.lens (\DeliveryStreamDescription' {versionId} -> versionId) (\s@DeliveryStreamDescription' {} a -> s {versionId = a} :: DeliveryStreamDescription)

-- | The destinations.
deliveryStreamDescription_destinations :: Lens.Lens' DeliveryStreamDescription [DestinationDescription]
deliveryStreamDescription_destinations = Lens.lens (\DeliveryStreamDescription' {destinations} -> destinations) (\s@DeliveryStreamDescription' {} a -> s {destinations = a} :: DeliveryStreamDescription) Core.. Lens._Coerce

-- | Indicates whether there are more destinations available to list.
deliveryStreamDescription_hasMoreDestinations :: Lens.Lens' DeliveryStreamDescription Core.Bool
deliveryStreamDescription_hasMoreDestinations = Lens.lens (\DeliveryStreamDescription' {hasMoreDestinations} -> hasMoreDestinations) (\s@DeliveryStreamDescription' {} a -> s {hasMoreDestinations = a} :: DeliveryStreamDescription)

instance Core.FromJSON DeliveryStreamDescription where
  parseJSON =
    Core.withObject
      "DeliveryStreamDescription"
      ( \x ->
          DeliveryStreamDescription'
            Core.<$> (x Core..:? "DeliveryStreamEncryptionConfiguration")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "CreateTimestamp")
            Core.<*> (x Core..:? "FailureDescription")
            Core.<*> (x Core..:? "LastUpdateTimestamp")
            Core.<*> (x Core..: "DeliveryStreamName")
            Core.<*> (x Core..: "DeliveryStreamARN")
            Core.<*> (x Core..: "DeliveryStreamStatus")
            Core.<*> (x Core..: "DeliveryStreamType")
            Core.<*> (x Core..: "VersionId")
            Core.<*> (x Core..:? "Destinations" Core..!= Core.mempty)
            Core.<*> (x Core..: "HasMoreDestinations")
      )

instance Core.Hashable DeliveryStreamDescription

instance Core.NFData DeliveryStreamDescription

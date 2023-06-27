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
-- Module      : Amazonka.Firehose.Types.DeliveryStreamDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DeliveryStreamDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Amazonka.Firehose.Types.DeliveryStreamStatus
import Amazonka.Firehose.Types.DeliveryStreamType
import Amazonka.Firehose.Types.DestinationDescription
import Amazonka.Firehose.Types.FailureDescription
import Amazonka.Firehose.Types.SourceDescription
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a delivery stream.
--
-- /See:/ 'newDeliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { -- | The date and time that the delivery stream was created.
    createTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Indicates the server-side encryption (SSE) status for the delivery
    -- stream.
    deliveryStreamEncryptionConfiguration :: Prelude.Maybe DeliveryStreamEncryptionConfiguration,
    -- | Provides details in case one of the following operations fails due to an
    -- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
    -- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
    failureDescription :: Prelude.Maybe FailureDescription,
    -- | The date and time that the delivery stream was last updated.
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
    -- SourceDescription object describing the source Kinesis data stream.
    source :: Prelude.Maybe SourceDescription,
    -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the delivery stream. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    deliveryStreamARN :: Prelude.Text,
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
    versionId :: Prelude.Text,
    -- | The destinations.
    destinations :: [DestinationDescription],
    -- | Indicates whether there are more destinations available to list.
    hasMoreDestinations :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliveryStreamDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTimestamp', 'deliveryStreamDescription_createTimestamp' - The date and time that the delivery stream was created.
--
-- 'deliveryStreamEncryptionConfiguration', 'deliveryStreamDescription_deliveryStreamEncryptionConfiguration' - Indicates the server-side encryption (SSE) status for the delivery
-- stream.
--
-- 'failureDescription', 'deliveryStreamDescription_failureDescription' - Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
--
-- 'lastUpdateTimestamp', 'deliveryStreamDescription_lastUpdateTimestamp' - The date and time that the delivery stream was last updated.
--
-- 'source', 'deliveryStreamDescription_source' - If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
-- SourceDescription object describing the source Kinesis data stream.
--
-- 'deliveryStreamName', 'deliveryStreamDescription_deliveryStreamName' - The name of the delivery stream.
--
-- 'deliveryStreamARN', 'deliveryStreamDescription_deliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
  Prelude.Text ->
  -- | 'deliveryStreamARN'
  Prelude.Text ->
  -- | 'deliveryStreamStatus'
  DeliveryStreamStatus ->
  -- | 'deliveryStreamType'
  DeliveryStreamType ->
  -- | 'versionId'
  Prelude.Text ->
  -- | 'hasMoreDestinations'
  Prelude.Bool ->
  DeliveryStreamDescription
newDeliveryStreamDescription
  pDeliveryStreamName_
  pDeliveryStreamARN_
  pDeliveryStreamStatus_
  pDeliveryStreamType_
  pVersionId_
  pHasMoreDestinations_ =
    DeliveryStreamDescription'
      { createTimestamp =
          Prelude.Nothing,
        deliveryStreamEncryptionConfiguration =
          Prelude.Nothing,
        failureDescription = Prelude.Nothing,
        lastUpdateTimestamp = Prelude.Nothing,
        source = Prelude.Nothing,
        deliveryStreamName = pDeliveryStreamName_,
        deliveryStreamARN = pDeliveryStreamARN_,
        deliveryStreamStatus = pDeliveryStreamStatus_,
        deliveryStreamType = pDeliveryStreamType_,
        versionId = pVersionId_,
        destinations = Prelude.mempty,
        hasMoreDestinations = pHasMoreDestinations_
      }

-- | The date and time that the delivery stream was created.
deliveryStreamDescription_createTimestamp :: Lens.Lens' DeliveryStreamDescription (Prelude.Maybe Prelude.UTCTime)
deliveryStreamDescription_createTimestamp = Lens.lens (\DeliveryStreamDescription' {createTimestamp} -> createTimestamp) (\s@DeliveryStreamDescription' {} a -> s {createTimestamp = a} :: DeliveryStreamDescription) Prelude.. Lens.mapping Data._Time

-- | Indicates the server-side encryption (SSE) status for the delivery
-- stream.
deliveryStreamDescription_deliveryStreamEncryptionConfiguration :: Lens.Lens' DeliveryStreamDescription (Prelude.Maybe DeliveryStreamEncryptionConfiguration)
deliveryStreamDescription_deliveryStreamEncryptionConfiguration = Lens.lens (\DeliveryStreamDescription' {deliveryStreamEncryptionConfiguration} -> deliveryStreamEncryptionConfiguration) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamEncryptionConfiguration = a} :: DeliveryStreamDescription)

-- | Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
deliveryStreamDescription_failureDescription :: Lens.Lens' DeliveryStreamDescription (Prelude.Maybe FailureDescription)
deliveryStreamDescription_failureDescription = Lens.lens (\DeliveryStreamDescription' {failureDescription} -> failureDescription) (\s@DeliveryStreamDescription' {} a -> s {failureDescription = a} :: DeliveryStreamDescription)

-- | The date and time that the delivery stream was last updated.
deliveryStreamDescription_lastUpdateTimestamp :: Lens.Lens' DeliveryStreamDescription (Prelude.Maybe Prelude.UTCTime)
deliveryStreamDescription_lastUpdateTimestamp = Lens.lens (\DeliveryStreamDescription' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@DeliveryStreamDescription' {} a -> s {lastUpdateTimestamp = a} :: DeliveryStreamDescription) Prelude.. Lens.mapping Data._Time

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@, a
-- SourceDescription object describing the source Kinesis data stream.
deliveryStreamDescription_source :: Lens.Lens' DeliveryStreamDescription (Prelude.Maybe SourceDescription)
deliveryStreamDescription_source = Lens.lens (\DeliveryStreamDescription' {source} -> source) (\s@DeliveryStreamDescription' {} a -> s {source = a} :: DeliveryStreamDescription)

-- | The name of the delivery stream.
deliveryStreamDescription_deliveryStreamName :: Lens.Lens' DeliveryStreamDescription Prelude.Text
deliveryStreamDescription_deliveryStreamName = Lens.lens (\DeliveryStreamDescription' {deliveryStreamName} -> deliveryStreamName) (\s@DeliveryStreamDescription' {} a -> s {deliveryStreamName = a} :: DeliveryStreamDescription)

-- | The Amazon Resource Name (ARN) of the delivery stream. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
deliveryStreamDescription_deliveryStreamARN :: Lens.Lens' DeliveryStreamDescription Prelude.Text
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
deliveryStreamDescription_versionId :: Lens.Lens' DeliveryStreamDescription Prelude.Text
deliveryStreamDescription_versionId = Lens.lens (\DeliveryStreamDescription' {versionId} -> versionId) (\s@DeliveryStreamDescription' {} a -> s {versionId = a} :: DeliveryStreamDescription)

-- | The destinations.
deliveryStreamDescription_destinations :: Lens.Lens' DeliveryStreamDescription [DestinationDescription]
deliveryStreamDescription_destinations = Lens.lens (\DeliveryStreamDescription' {destinations} -> destinations) (\s@DeliveryStreamDescription' {} a -> s {destinations = a} :: DeliveryStreamDescription) Prelude.. Lens.coerced

-- | Indicates whether there are more destinations available to list.
deliveryStreamDescription_hasMoreDestinations :: Lens.Lens' DeliveryStreamDescription Prelude.Bool
deliveryStreamDescription_hasMoreDestinations = Lens.lens (\DeliveryStreamDescription' {hasMoreDestinations} -> hasMoreDestinations) (\s@DeliveryStreamDescription' {} a -> s {hasMoreDestinations = a} :: DeliveryStreamDescription)

instance Data.FromJSON DeliveryStreamDescription where
  parseJSON =
    Data.withObject
      "DeliveryStreamDescription"
      ( \x ->
          DeliveryStreamDescription'
            Prelude.<$> (x Data..:? "CreateTimestamp")
            Prelude.<*> (x Data..:? "DeliveryStreamEncryptionConfiguration")
            Prelude.<*> (x Data..:? "FailureDescription")
            Prelude.<*> (x Data..:? "LastUpdateTimestamp")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..: "DeliveryStreamName")
            Prelude.<*> (x Data..: "DeliveryStreamARN")
            Prelude.<*> (x Data..: "DeliveryStreamStatus")
            Prelude.<*> (x Data..: "DeliveryStreamType")
            Prelude.<*> (x Data..: "VersionId")
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "HasMoreDestinations")
      )

instance Prelude.Hashable DeliveryStreamDescription where
  hashWithSalt _salt DeliveryStreamDescription' {..} =
    _salt
      `Prelude.hashWithSalt` createTimestamp
      `Prelude.hashWithSalt` deliveryStreamEncryptionConfiguration
      `Prelude.hashWithSalt` failureDescription
      `Prelude.hashWithSalt` lastUpdateTimestamp
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` deliveryStreamName
      `Prelude.hashWithSalt` deliveryStreamARN
      `Prelude.hashWithSalt` deliveryStreamStatus
      `Prelude.hashWithSalt` deliveryStreamType
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` hasMoreDestinations

instance Prelude.NFData DeliveryStreamDescription where
  rnf DeliveryStreamDescription' {..} =
    Prelude.rnf createTimestamp
      `Prelude.seq` Prelude.rnf deliveryStreamEncryptionConfiguration
      `Prelude.seq` Prelude.rnf failureDescription
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf deliveryStreamName
      `Prelude.seq` Prelude.rnf deliveryStreamARN
      `Prelude.seq` Prelude.rnf deliveryStreamStatus
      `Prelude.seq` Prelude.rnf deliveryStreamType
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf hasMoreDestinations

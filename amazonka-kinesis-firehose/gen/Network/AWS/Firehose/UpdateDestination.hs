{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.UpdateDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified destination of the specified delivery stream.
--
-- Use this operation to change the destination type (for example, to
-- replace the Amazon S3 destination with Amazon Redshift) or change the
-- parameters associated with a destination (for example, to change the
-- bucket name of the Amazon S3 destination). The update might not occur
-- immediately. The target delivery stream remains active while the
-- configurations are updated, so data writes to the delivery stream can
-- continue during this process. The updated configurations are usually
-- effective within a few minutes.
--
-- Switching between Amazon ES and other services is not supported. For an
-- Amazon ES destination, you can only update to another Amazon ES
-- destination.
--
-- If the destination type is the same, Kinesis Data Firehose merges the
-- configuration parameters specified with the destination configuration
-- that already exists on the delivery stream. If any of the parameters are
-- not specified in the call, the existing values are retained. For
-- example, in the Amazon S3 destination, if EncryptionConfiguration is not
-- specified, then the existing @EncryptionConfiguration@ is maintained on
-- the destination.
--
-- If the destination type is not the same, for example, changing the
-- destination from Amazon S3 to Amazon Redshift, Kinesis Data Firehose
-- does not merge any parameters. In this case, all parameters must be
-- specified.
--
-- Kinesis Data Firehose uses @CurrentDeliveryStreamVersionId@ to avoid
-- race conditions and conflicting merges. This is a required field, and
-- the service updates the configuration only if the existing configuration
-- has a version ID that matches. After the update is applied successfully,
-- the version ID is updated, and can be retrieved using
-- DescribeDeliveryStream. Use the new version ID to set
-- @CurrentDeliveryStreamVersionId@ in the next call.
module Network.AWS.Firehose.UpdateDestination
  ( -- * Creating a Request
    UpdateDestination (..),
    newUpdateDestination,

    -- * Request Lenses
    updateDestination_redshiftDestinationUpdate,
    updateDestination_s3DestinationUpdate,
    updateDestination_extendedS3DestinationUpdate,
    updateDestination_httpEndpointDestinationUpdate,
    updateDestination_elasticsearchDestinationUpdate,
    updateDestination_splunkDestinationUpdate,
    updateDestination_deliveryStreamName,
    updateDestination_currentDeliveryStreamVersionId,
    updateDestination_destinationId,

    -- * Destructuring the Response
    UpdateDestinationResponse (..),
    newUpdateDestinationResponse,

    -- * Response Lenses
    updateDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { -- | Describes an update for a destination in Amazon Redshift.
    redshiftDestinationUpdate :: Core.Maybe RedshiftDestinationUpdate,
    -- | [Deprecated] Describes an update for a destination in Amazon S3.
    s3DestinationUpdate :: Core.Maybe S3DestinationUpdate,
    -- | Describes an update for a destination in Amazon S3.
    extendedS3DestinationUpdate :: Core.Maybe ExtendedS3DestinationUpdate,
    -- | Describes an update to the specified HTTP endpoint destination.
    httpEndpointDestinationUpdate :: Core.Maybe HttpEndpointDestinationUpdate,
    -- | Describes an update for a destination in Amazon ES.
    elasticsearchDestinationUpdate :: Core.Maybe ElasticsearchDestinationUpdate,
    -- | Describes an update for a destination in Splunk.
    splunkDestinationUpdate :: Core.Maybe SplunkDestinationUpdate,
    -- | The name of the delivery stream.
    deliveryStreamName :: Core.Text,
    -- | Obtain this value from the @VersionId@ result of
    -- DeliveryStreamDescription. This value is required, and helps the service
    -- perform conditional operations. For example, if there is an interleaving
    -- update and this value is null, then the update destination fails. After
    -- the update is successful, the @VersionId@ value is updated. The service
    -- then performs a merge of the old configuration with the new
    -- configuration.
    currentDeliveryStreamVersionId :: Core.Text,
    -- | The ID of the destination.
    destinationId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redshiftDestinationUpdate', 'updateDestination_redshiftDestinationUpdate' - Describes an update for a destination in Amazon Redshift.
--
-- 's3DestinationUpdate', 'updateDestination_s3DestinationUpdate' - [Deprecated] Describes an update for a destination in Amazon S3.
--
-- 'extendedS3DestinationUpdate', 'updateDestination_extendedS3DestinationUpdate' - Describes an update for a destination in Amazon S3.
--
-- 'httpEndpointDestinationUpdate', 'updateDestination_httpEndpointDestinationUpdate' - Describes an update to the specified HTTP endpoint destination.
--
-- 'elasticsearchDestinationUpdate', 'updateDestination_elasticsearchDestinationUpdate' - Describes an update for a destination in Amazon ES.
--
-- 'splunkDestinationUpdate', 'updateDestination_splunkDestinationUpdate' - Describes an update for a destination in Splunk.
--
-- 'deliveryStreamName', 'updateDestination_deliveryStreamName' - The name of the delivery stream.
--
-- 'currentDeliveryStreamVersionId', 'updateDestination_currentDeliveryStreamVersionId' - Obtain this value from the @VersionId@ result of
-- DeliveryStreamDescription. This value is required, and helps the service
-- perform conditional operations. For example, if there is an interleaving
-- update and this value is null, then the update destination fails. After
-- the update is successful, the @VersionId@ value is updated. The service
-- then performs a merge of the old configuration with the new
-- configuration.
--
-- 'destinationId', 'updateDestination_destinationId' - The ID of the destination.
newUpdateDestination ::
  -- | 'deliveryStreamName'
  Core.Text ->
  -- | 'currentDeliveryStreamVersionId'
  Core.Text ->
  -- | 'destinationId'
  Core.Text ->
  UpdateDestination
newUpdateDestination
  pDeliveryStreamName_
  pCurrentDeliveryStreamVersionId_
  pDestinationId_ =
    UpdateDestination'
      { redshiftDestinationUpdate =
          Core.Nothing,
        s3DestinationUpdate = Core.Nothing,
        extendedS3DestinationUpdate = Core.Nothing,
        httpEndpointDestinationUpdate = Core.Nothing,
        elasticsearchDestinationUpdate = Core.Nothing,
        splunkDestinationUpdate = Core.Nothing,
        deliveryStreamName = pDeliveryStreamName_,
        currentDeliveryStreamVersionId =
          pCurrentDeliveryStreamVersionId_,
        destinationId = pDestinationId_
      }

-- | Describes an update for a destination in Amazon Redshift.
updateDestination_redshiftDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe RedshiftDestinationUpdate)
updateDestination_redshiftDestinationUpdate = Lens.lens (\UpdateDestination' {redshiftDestinationUpdate} -> redshiftDestinationUpdate) (\s@UpdateDestination' {} a -> s {redshiftDestinationUpdate = a} :: UpdateDestination)

-- | [Deprecated] Describes an update for a destination in Amazon S3.
updateDestination_s3DestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe S3DestinationUpdate)
updateDestination_s3DestinationUpdate = Lens.lens (\UpdateDestination' {s3DestinationUpdate} -> s3DestinationUpdate) (\s@UpdateDestination' {} a -> s {s3DestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Amazon S3.
updateDestination_extendedS3DestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe ExtendedS3DestinationUpdate)
updateDestination_extendedS3DestinationUpdate = Lens.lens (\UpdateDestination' {extendedS3DestinationUpdate} -> extendedS3DestinationUpdate) (\s@UpdateDestination' {} a -> s {extendedS3DestinationUpdate = a} :: UpdateDestination)

-- | Describes an update to the specified HTTP endpoint destination.
updateDestination_httpEndpointDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe HttpEndpointDestinationUpdate)
updateDestination_httpEndpointDestinationUpdate = Lens.lens (\UpdateDestination' {httpEndpointDestinationUpdate} -> httpEndpointDestinationUpdate) (\s@UpdateDestination' {} a -> s {httpEndpointDestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Amazon ES.
updateDestination_elasticsearchDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe ElasticsearchDestinationUpdate)
updateDestination_elasticsearchDestinationUpdate = Lens.lens (\UpdateDestination' {elasticsearchDestinationUpdate} -> elasticsearchDestinationUpdate) (\s@UpdateDestination' {} a -> s {elasticsearchDestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Splunk.
updateDestination_splunkDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe SplunkDestinationUpdate)
updateDestination_splunkDestinationUpdate = Lens.lens (\UpdateDestination' {splunkDestinationUpdate} -> splunkDestinationUpdate) (\s@UpdateDestination' {} a -> s {splunkDestinationUpdate = a} :: UpdateDestination)

-- | The name of the delivery stream.
updateDestination_deliveryStreamName :: Lens.Lens' UpdateDestination Core.Text
updateDestination_deliveryStreamName = Lens.lens (\UpdateDestination' {deliveryStreamName} -> deliveryStreamName) (\s@UpdateDestination' {} a -> s {deliveryStreamName = a} :: UpdateDestination)

-- | Obtain this value from the @VersionId@ result of
-- DeliveryStreamDescription. This value is required, and helps the service
-- perform conditional operations. For example, if there is an interleaving
-- update and this value is null, then the update destination fails. After
-- the update is successful, the @VersionId@ value is updated. The service
-- then performs a merge of the old configuration with the new
-- configuration.
updateDestination_currentDeliveryStreamVersionId :: Lens.Lens' UpdateDestination Core.Text
updateDestination_currentDeliveryStreamVersionId = Lens.lens (\UpdateDestination' {currentDeliveryStreamVersionId} -> currentDeliveryStreamVersionId) (\s@UpdateDestination' {} a -> s {currentDeliveryStreamVersionId = a} :: UpdateDestination)

-- | The ID of the destination.
updateDestination_destinationId :: Lens.Lens' UpdateDestination Core.Text
updateDestination_destinationId = Lens.lens (\UpdateDestination' {destinationId} -> destinationId) (\s@UpdateDestination' {} a -> s {destinationId = a} :: UpdateDestination)

instance Core.AWSRequest UpdateDestination where
  type
    AWSResponse UpdateDestination =
      UpdateDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDestination

instance Core.NFData UpdateDestination

instance Core.ToHeaders UpdateDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.UpdateDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDestination where
  toJSON UpdateDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RedshiftDestinationUpdate" Core..=)
              Core.<$> redshiftDestinationUpdate,
            ("S3DestinationUpdate" Core..=)
              Core.<$> s3DestinationUpdate,
            ("ExtendedS3DestinationUpdate" Core..=)
              Core.<$> extendedS3DestinationUpdate,
            ("HttpEndpointDestinationUpdate" Core..=)
              Core.<$> httpEndpointDestinationUpdate,
            ("ElasticsearchDestinationUpdate" Core..=)
              Core.<$> elasticsearchDestinationUpdate,
            ("SplunkDestinationUpdate" Core..=)
              Core.<$> splunkDestinationUpdate,
            Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just
              ( "CurrentDeliveryStreamVersionId"
                  Core..= currentDeliveryStreamVersionId
              ),
            Core.Just ("DestinationId" Core..= destinationId)
          ]
      )

instance Core.ToPath UpdateDestination where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDestinationResponse' smart constructor.
data UpdateDestinationResponse = UpdateDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDestinationResponse_httpStatus' - The response's http status code.
newUpdateDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDestinationResponse
newUpdateDestinationResponse pHttpStatus_ =
  UpdateDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDestinationResponse_httpStatus :: Lens.Lens' UpdateDestinationResponse Core.Int
updateDestinationResponse_httpStatus = Lens.lens (\UpdateDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateDestinationResponse' {} a -> s {httpStatus = a} :: UpdateDestinationResponse)

instance Core.NFData UpdateDestinationResponse

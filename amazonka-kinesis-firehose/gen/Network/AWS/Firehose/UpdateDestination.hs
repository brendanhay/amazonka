{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { -- | Describes an update for a destination in Amazon Redshift.
    redshiftDestinationUpdate :: Prelude.Maybe RedshiftDestinationUpdate,
    -- | [Deprecated] Describes an update for a destination in Amazon S3.
    s3DestinationUpdate :: Prelude.Maybe S3DestinationUpdate,
    -- | Describes an update for a destination in Amazon S3.
    extendedS3DestinationUpdate :: Prelude.Maybe ExtendedS3DestinationUpdate,
    -- | Describes an update to the specified HTTP endpoint destination.
    httpEndpointDestinationUpdate :: Prelude.Maybe HttpEndpointDestinationUpdate,
    -- | Describes an update for a destination in Amazon ES.
    elasticsearchDestinationUpdate :: Prelude.Maybe ElasticsearchDestinationUpdate,
    -- | Describes an update for a destination in Splunk.
    splunkDestinationUpdate :: Prelude.Maybe SplunkDestinationUpdate,
    -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text,
    -- | Obtain this value from the @VersionId@ result of
    -- DeliveryStreamDescription. This value is required, and helps the service
    -- perform conditional operations. For example, if there is an interleaving
    -- update and this value is null, then the update destination fails. After
    -- the update is successful, the @VersionId@ value is updated. The service
    -- then performs a merge of the old configuration with the new
    -- configuration.
    currentDeliveryStreamVersionId :: Prelude.Text,
    -- | The ID of the destination.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'currentDeliveryStreamVersionId'
  Prelude.Text ->
  -- | 'destinationId'
  Prelude.Text ->
  UpdateDestination
newUpdateDestination
  pDeliveryStreamName_
  pCurrentDeliveryStreamVersionId_
  pDestinationId_ =
    UpdateDestination'
      { redshiftDestinationUpdate =
          Prelude.Nothing,
        s3DestinationUpdate = Prelude.Nothing,
        extendedS3DestinationUpdate = Prelude.Nothing,
        httpEndpointDestinationUpdate = Prelude.Nothing,
        elasticsearchDestinationUpdate = Prelude.Nothing,
        splunkDestinationUpdate = Prelude.Nothing,
        deliveryStreamName = pDeliveryStreamName_,
        currentDeliveryStreamVersionId =
          pCurrentDeliveryStreamVersionId_,
        destinationId = pDestinationId_
      }

-- | Describes an update for a destination in Amazon Redshift.
updateDestination_redshiftDestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe RedshiftDestinationUpdate)
updateDestination_redshiftDestinationUpdate = Lens.lens (\UpdateDestination' {redshiftDestinationUpdate} -> redshiftDestinationUpdate) (\s@UpdateDestination' {} a -> s {redshiftDestinationUpdate = a} :: UpdateDestination)

-- | [Deprecated] Describes an update for a destination in Amazon S3.
updateDestination_s3DestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe S3DestinationUpdate)
updateDestination_s3DestinationUpdate = Lens.lens (\UpdateDestination' {s3DestinationUpdate} -> s3DestinationUpdate) (\s@UpdateDestination' {} a -> s {s3DestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Amazon S3.
updateDestination_extendedS3DestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe ExtendedS3DestinationUpdate)
updateDestination_extendedS3DestinationUpdate = Lens.lens (\UpdateDestination' {extendedS3DestinationUpdate} -> extendedS3DestinationUpdate) (\s@UpdateDestination' {} a -> s {extendedS3DestinationUpdate = a} :: UpdateDestination)

-- | Describes an update to the specified HTTP endpoint destination.
updateDestination_httpEndpointDestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe HttpEndpointDestinationUpdate)
updateDestination_httpEndpointDestinationUpdate = Lens.lens (\UpdateDestination' {httpEndpointDestinationUpdate} -> httpEndpointDestinationUpdate) (\s@UpdateDestination' {} a -> s {httpEndpointDestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Amazon ES.
updateDestination_elasticsearchDestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe ElasticsearchDestinationUpdate)
updateDestination_elasticsearchDestinationUpdate = Lens.lens (\UpdateDestination' {elasticsearchDestinationUpdate} -> elasticsearchDestinationUpdate) (\s@UpdateDestination' {} a -> s {elasticsearchDestinationUpdate = a} :: UpdateDestination)

-- | Describes an update for a destination in Splunk.
updateDestination_splunkDestinationUpdate :: Lens.Lens' UpdateDestination (Prelude.Maybe SplunkDestinationUpdate)
updateDestination_splunkDestinationUpdate = Lens.lens (\UpdateDestination' {splunkDestinationUpdate} -> splunkDestinationUpdate) (\s@UpdateDestination' {} a -> s {splunkDestinationUpdate = a} :: UpdateDestination)

-- | The name of the delivery stream.
updateDestination_deliveryStreamName :: Lens.Lens' UpdateDestination Prelude.Text
updateDestination_deliveryStreamName = Lens.lens (\UpdateDestination' {deliveryStreamName} -> deliveryStreamName) (\s@UpdateDestination' {} a -> s {deliveryStreamName = a} :: UpdateDestination)

-- | Obtain this value from the @VersionId@ result of
-- DeliveryStreamDescription. This value is required, and helps the service
-- perform conditional operations. For example, if there is an interleaving
-- update and this value is null, then the update destination fails. After
-- the update is successful, the @VersionId@ value is updated. The service
-- then performs a merge of the old configuration with the new
-- configuration.
updateDestination_currentDeliveryStreamVersionId :: Lens.Lens' UpdateDestination Prelude.Text
updateDestination_currentDeliveryStreamVersionId = Lens.lens (\UpdateDestination' {currentDeliveryStreamVersionId} -> currentDeliveryStreamVersionId) (\s@UpdateDestination' {} a -> s {currentDeliveryStreamVersionId = a} :: UpdateDestination)

-- | The ID of the destination.
updateDestination_destinationId :: Lens.Lens' UpdateDestination Prelude.Text
updateDestination_destinationId = Lens.lens (\UpdateDestination' {destinationId} -> destinationId) (\s@UpdateDestination' {} a -> s {destinationId = a} :: UpdateDestination)

instance Prelude.AWSRequest UpdateDestination where
  type Rs UpdateDestination = UpdateDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDestination

instance Prelude.NFData UpdateDestination

instance Prelude.ToHeaders UpdateDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Firehose_20150804.UpdateDestination" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDestination where
  toJSON UpdateDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RedshiftDestinationUpdate" Prelude..=)
              Prelude.<$> redshiftDestinationUpdate,
            ("S3DestinationUpdate" Prelude..=)
              Prelude.<$> s3DestinationUpdate,
            ("ExtendedS3DestinationUpdate" Prelude..=)
              Prelude.<$> extendedS3DestinationUpdate,
            ("HttpEndpointDestinationUpdate" Prelude..=)
              Prelude.<$> httpEndpointDestinationUpdate,
            ("ElasticsearchDestinationUpdate" Prelude..=)
              Prelude.<$> elasticsearchDestinationUpdate,
            ("SplunkDestinationUpdate" Prelude..=)
              Prelude.<$> splunkDestinationUpdate,
            Prelude.Just
              ("DeliveryStreamName" Prelude..= deliveryStreamName),
            Prelude.Just
              ( "CurrentDeliveryStreamVersionId"
                  Prelude..= currentDeliveryStreamVersionId
              ),
            Prelude.Just
              ("DestinationId" Prelude..= destinationId)
          ]
      )

instance Prelude.ToPath UpdateDestination where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDestinationResponse' smart constructor.
data UpdateDestinationResponse = UpdateDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDestinationResponse
newUpdateDestinationResponse pHttpStatus_ =
  UpdateDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDestinationResponse_httpStatus :: Lens.Lens' UpdateDestinationResponse Prelude.Int
updateDestinationResponse_httpStatus = Lens.lens (\UpdateDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateDestinationResponse' {} a -> s {httpStatus = a} :: UpdateDestinationResponse)

instance Prelude.NFData UpdateDestinationResponse

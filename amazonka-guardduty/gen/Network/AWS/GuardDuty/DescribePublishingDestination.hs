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
-- Module      : Network.AWS.GuardDuty.DescribePublishingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the publishing destination specified by the
-- provided @destinationId@.
module Network.AWS.GuardDuty.DescribePublishingDestination
  ( -- * Creating a Request
    DescribePublishingDestination (..),
    newDescribePublishingDestination,

    -- * Request Lenses
    describePublishingDestination_detectorId,
    describePublishingDestination_destinationId,

    -- * Destructuring the Response
    DescribePublishingDestinationResponse (..),
    newDescribePublishingDestinationResponse,

    -- * Response Lenses
    describePublishingDestinationResponse_httpStatus,
    describePublishingDestinationResponse_destinationId,
    describePublishingDestinationResponse_destinationType,
    describePublishingDestinationResponse_status,
    describePublishingDestinationResponse_publishingFailureStartTimestamp,
    describePublishingDestinationResponse_destinationProperties,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { -- | The unique ID of the detector associated with the publishing destination
    -- to retrieve.
    detectorId :: Core.Text,
    -- | The ID of the publishing destination to retrieve.
    destinationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePublishingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'describePublishingDestination_detectorId' - The unique ID of the detector associated with the publishing destination
-- to retrieve.
--
-- 'destinationId', 'describePublishingDestination_destinationId' - The ID of the publishing destination to retrieve.
newDescribePublishingDestination ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'destinationId'
  Core.Text ->
  DescribePublishingDestination
newDescribePublishingDestination
  pDetectorId_
  pDestinationId_ =
    DescribePublishingDestination'
      { detectorId =
          pDetectorId_,
        destinationId = pDestinationId_
      }

-- | The unique ID of the detector associated with the publishing destination
-- to retrieve.
describePublishingDestination_detectorId :: Lens.Lens' DescribePublishingDestination Core.Text
describePublishingDestination_detectorId = Lens.lens (\DescribePublishingDestination' {detectorId} -> detectorId) (\s@DescribePublishingDestination' {} a -> s {detectorId = a} :: DescribePublishingDestination)

-- | The ID of the publishing destination to retrieve.
describePublishingDestination_destinationId :: Lens.Lens' DescribePublishingDestination Core.Text
describePublishingDestination_destinationId = Lens.lens (\DescribePublishingDestination' {destinationId} -> destinationId) (\s@DescribePublishingDestination' {} a -> s {destinationId = a} :: DescribePublishingDestination)

instance
  Core.AWSRequest
    DescribePublishingDestination
  where
  type
    AWSResponse DescribePublishingDestination =
      DescribePublishingDestinationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePublishingDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "destinationId")
            Core.<*> (x Core..:> "destinationType")
            Core.<*> (x Core..:> "status")
            Core.<*> (x Core..:> "publishingFailureStartTimestamp")
            Core.<*> (x Core..:> "destinationProperties")
      )

instance Core.Hashable DescribePublishingDestination

instance Core.NFData DescribePublishingDestination

instance Core.ToHeaders DescribePublishingDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribePublishingDestination where
  toPath DescribePublishingDestination' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/publishingDestination/",
        Core.toBS destinationId
      ]

instance Core.ToQuery DescribePublishingDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePublishingDestinationResponse' smart constructor.
data DescribePublishingDestinationResponse = DescribePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the publishing destination.
    destinationId :: Core.Text,
    -- | The type of publishing destination. Currently, only Amazon S3 buckets
    -- are supported.
    destinationType :: DestinationType,
    -- | The status of the publishing destination.
    status :: PublishingStatus,
    -- | The time, in epoch millisecond format, at which GuardDuty was first
    -- unable to publish findings to the destination.
    publishingFailureStartTimestamp :: Core.Integer,
    -- | A @DestinationProperties@ object that includes the @DestinationArn@ and
    -- @KmsKeyArn@ of the publishing destination.
    destinationProperties :: DestinationProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePublishingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describePublishingDestinationResponse_httpStatus' - The response's http status code.
--
-- 'destinationId', 'describePublishingDestinationResponse_destinationId' - The ID of the publishing destination.
--
-- 'destinationType', 'describePublishingDestinationResponse_destinationType' - The type of publishing destination. Currently, only Amazon S3 buckets
-- are supported.
--
-- 'status', 'describePublishingDestinationResponse_status' - The status of the publishing destination.
--
-- 'publishingFailureStartTimestamp', 'describePublishingDestinationResponse_publishingFailureStartTimestamp' - The time, in epoch millisecond format, at which GuardDuty was first
-- unable to publish findings to the destination.
--
-- 'destinationProperties', 'describePublishingDestinationResponse_destinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and
-- @KmsKeyArn@ of the publishing destination.
newDescribePublishingDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'destinationId'
  Core.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'status'
  PublishingStatus ->
  -- | 'publishingFailureStartTimestamp'
  Core.Integer ->
  -- | 'destinationProperties'
  DestinationProperties ->
  DescribePublishingDestinationResponse
newDescribePublishingDestinationResponse
  pHttpStatus_
  pDestinationId_
  pDestinationType_
  pStatus_
  pPublishingFailureStartTimestamp_
  pDestinationProperties_ =
    DescribePublishingDestinationResponse'
      { httpStatus =
          pHttpStatus_,
        destinationId = pDestinationId_,
        destinationType = pDestinationType_,
        status = pStatus_,
        publishingFailureStartTimestamp =
          pPublishingFailureStartTimestamp_,
        destinationProperties =
          pDestinationProperties_
      }

-- | The response's http status code.
describePublishingDestinationResponse_httpStatus :: Lens.Lens' DescribePublishingDestinationResponse Core.Int
describePublishingDestinationResponse_httpStatus = Lens.lens (\DescribePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@DescribePublishingDestinationResponse' {} a -> s {httpStatus = a} :: DescribePublishingDestinationResponse)

-- | The ID of the publishing destination.
describePublishingDestinationResponse_destinationId :: Lens.Lens' DescribePublishingDestinationResponse Core.Text
describePublishingDestinationResponse_destinationId = Lens.lens (\DescribePublishingDestinationResponse' {destinationId} -> destinationId) (\s@DescribePublishingDestinationResponse' {} a -> s {destinationId = a} :: DescribePublishingDestinationResponse)

-- | The type of publishing destination. Currently, only Amazon S3 buckets
-- are supported.
describePublishingDestinationResponse_destinationType :: Lens.Lens' DescribePublishingDestinationResponse DestinationType
describePublishingDestinationResponse_destinationType = Lens.lens (\DescribePublishingDestinationResponse' {destinationType} -> destinationType) (\s@DescribePublishingDestinationResponse' {} a -> s {destinationType = a} :: DescribePublishingDestinationResponse)

-- | The status of the publishing destination.
describePublishingDestinationResponse_status :: Lens.Lens' DescribePublishingDestinationResponse PublishingStatus
describePublishingDestinationResponse_status = Lens.lens (\DescribePublishingDestinationResponse' {status} -> status) (\s@DescribePublishingDestinationResponse' {} a -> s {status = a} :: DescribePublishingDestinationResponse)

-- | The time, in epoch millisecond format, at which GuardDuty was first
-- unable to publish findings to the destination.
describePublishingDestinationResponse_publishingFailureStartTimestamp :: Lens.Lens' DescribePublishingDestinationResponse Core.Integer
describePublishingDestinationResponse_publishingFailureStartTimestamp = Lens.lens (\DescribePublishingDestinationResponse' {publishingFailureStartTimestamp} -> publishingFailureStartTimestamp) (\s@DescribePublishingDestinationResponse' {} a -> s {publishingFailureStartTimestamp = a} :: DescribePublishingDestinationResponse)

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and
-- @KmsKeyArn@ of the publishing destination.
describePublishingDestinationResponse_destinationProperties :: Lens.Lens' DescribePublishingDestinationResponse DestinationProperties
describePublishingDestinationResponse_destinationProperties = Lens.lens (\DescribePublishingDestinationResponse' {destinationProperties} -> destinationProperties) (\s@DescribePublishingDestinationResponse' {} a -> s {destinationProperties = a} :: DescribePublishingDestinationResponse)

instance
  Core.NFData
    DescribePublishingDestinationResponse

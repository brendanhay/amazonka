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
-- Module      : Network.AWS.GuardDuty.CreatePublishingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a publishing destination to export findings to. The resource to
-- export findings to must exist before you use this operation.
module Network.AWS.GuardDuty.CreatePublishingDestination
  ( -- * Creating a Request
    CreatePublishingDestination (..),
    newCreatePublishingDestination,

    -- * Request Lenses
    createPublishingDestination_clientToken,
    createPublishingDestination_detectorId,
    createPublishingDestination_destinationType,
    createPublishingDestination_destinationProperties,

    -- * Destructuring the Response
    CreatePublishingDestinationResponse (..),
    newCreatePublishingDestinationResponse,

    -- * Response Lenses
    createPublishingDestinationResponse_httpStatus,
    createPublishingDestinationResponse_destinationId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { -- | The idempotency token for the request.
    clientToken :: Core.Maybe Core.Text,
    -- | The ID of the GuardDuty detector associated with the publishing
    -- destination.
    detectorId :: Core.Text,
    -- | The type of resource for the publishing destination. Currently only
    -- Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | The properties of the publishing destination, including the ARNs for the
    -- destination and the KMS key used for encryption.
    destinationProperties :: DestinationProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePublishingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPublishingDestination_clientToken' - The idempotency token for the request.
--
-- 'detectorId', 'createPublishingDestination_detectorId' - The ID of the GuardDuty detector associated with the publishing
-- destination.
--
-- 'destinationType', 'createPublishingDestination_destinationType' - The type of resource for the publishing destination. Currently only
-- Amazon S3 buckets are supported.
--
-- 'destinationProperties', 'createPublishingDestination_destinationProperties' - The properties of the publishing destination, including the ARNs for the
-- destination and the KMS key used for encryption.
newCreatePublishingDestination ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'destinationProperties'
  DestinationProperties ->
  CreatePublishingDestination
newCreatePublishingDestination
  pDetectorId_
  pDestinationType_
  pDestinationProperties_ =
    CreatePublishingDestination'
      { clientToken =
          Core.Nothing,
        detectorId = pDetectorId_,
        destinationType = pDestinationType_,
        destinationProperties =
          pDestinationProperties_
      }

-- | The idempotency token for the request.
createPublishingDestination_clientToken :: Lens.Lens' CreatePublishingDestination (Core.Maybe Core.Text)
createPublishingDestination_clientToken = Lens.lens (\CreatePublishingDestination' {clientToken} -> clientToken) (\s@CreatePublishingDestination' {} a -> s {clientToken = a} :: CreatePublishingDestination)

-- | The ID of the GuardDuty detector associated with the publishing
-- destination.
createPublishingDestination_detectorId :: Lens.Lens' CreatePublishingDestination Core.Text
createPublishingDestination_detectorId = Lens.lens (\CreatePublishingDestination' {detectorId} -> detectorId) (\s@CreatePublishingDestination' {} a -> s {detectorId = a} :: CreatePublishingDestination)

-- | The type of resource for the publishing destination. Currently only
-- Amazon S3 buckets are supported.
createPublishingDestination_destinationType :: Lens.Lens' CreatePublishingDestination DestinationType
createPublishingDestination_destinationType = Lens.lens (\CreatePublishingDestination' {destinationType} -> destinationType) (\s@CreatePublishingDestination' {} a -> s {destinationType = a} :: CreatePublishingDestination)

-- | The properties of the publishing destination, including the ARNs for the
-- destination and the KMS key used for encryption.
createPublishingDestination_destinationProperties :: Lens.Lens' CreatePublishingDestination DestinationProperties
createPublishingDestination_destinationProperties = Lens.lens (\CreatePublishingDestination' {destinationProperties} -> destinationProperties) (\s@CreatePublishingDestination' {} a -> s {destinationProperties = a} :: CreatePublishingDestination)

instance Core.AWSRequest CreatePublishingDestination where
  type
    AWSResponse CreatePublishingDestination =
      CreatePublishingDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePublishingDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "destinationId")
      )

instance Core.Hashable CreatePublishingDestination

instance Core.NFData CreatePublishingDestination

instance Core.ToHeaders CreatePublishingDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePublishingDestination where
  toJSON CreatePublishingDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientToken" Core..=) Core.<$> clientToken,
            Core.Just
              ("destinationType" Core..= destinationType),
            Core.Just
              ( "destinationProperties"
                  Core..= destinationProperties
              )
          ]
      )

instance Core.ToPath CreatePublishingDestination where
  toPath CreatePublishingDestination' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/publishingDestination"
      ]

instance Core.ToQuery CreatePublishingDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePublishingDestinationResponse' smart constructor.
data CreatePublishingDestinationResponse = CreatePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the publishing destination that is created.
    destinationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePublishingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPublishingDestinationResponse_httpStatus' - The response's http status code.
--
-- 'destinationId', 'createPublishingDestinationResponse_destinationId' - The ID of the publishing destination that is created.
newCreatePublishingDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'destinationId'
  Core.Text ->
  CreatePublishingDestinationResponse
newCreatePublishingDestinationResponse
  pHttpStatus_
  pDestinationId_ =
    CreatePublishingDestinationResponse'
      { httpStatus =
          pHttpStatus_,
        destinationId = pDestinationId_
      }

-- | The response's http status code.
createPublishingDestinationResponse_httpStatus :: Lens.Lens' CreatePublishingDestinationResponse Core.Int
createPublishingDestinationResponse_httpStatus = Lens.lens (\CreatePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@CreatePublishingDestinationResponse' {} a -> s {httpStatus = a} :: CreatePublishingDestinationResponse)

-- | The ID of the publishing destination that is created.
createPublishingDestinationResponse_destinationId :: Lens.Lens' CreatePublishingDestinationResponse Core.Text
createPublishingDestinationResponse_destinationId = Lens.lens (\CreatePublishingDestinationResponse' {destinationId} -> destinationId) (\s@CreatePublishingDestinationResponse' {} a -> s {destinationId = a} :: CreatePublishingDestinationResponse)

instance
  Core.NFData
    CreatePublishingDestinationResponse

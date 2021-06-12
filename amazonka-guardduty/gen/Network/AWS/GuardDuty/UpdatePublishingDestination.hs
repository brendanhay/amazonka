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
-- Module      : Network.AWS.GuardDuty.UpdatePublishingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the publishing destination specified by the
-- @destinationId@.
module Network.AWS.GuardDuty.UpdatePublishingDestination
  ( -- * Creating a Request
    UpdatePublishingDestination (..),
    newUpdatePublishingDestination,

    -- * Request Lenses
    updatePublishingDestination_destinationProperties,
    updatePublishingDestination_detectorId,
    updatePublishingDestination_destinationId,

    -- * Destructuring the Response
    UpdatePublishingDestinationResponse (..),
    newUpdatePublishingDestinationResponse,

    -- * Response Lenses
    updatePublishingDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePublishingDestination' smart constructor.
data UpdatePublishingDestination = UpdatePublishingDestination'
  { -- | A @DestinationProperties@ object that includes the @DestinationArn@ and
    -- @KmsKeyArn@ of the publishing destination.
    destinationProperties :: Core.Maybe DestinationProperties,
    -- | The ID of the detector associated with the publishing destinations to
    -- update.
    detectorId :: Core.Text,
    -- | The ID of the publishing destination to update.
    destinationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePublishingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationProperties', 'updatePublishingDestination_destinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and
-- @KmsKeyArn@ of the publishing destination.
--
-- 'detectorId', 'updatePublishingDestination_detectorId' - The ID of the detector associated with the publishing destinations to
-- update.
--
-- 'destinationId', 'updatePublishingDestination_destinationId' - The ID of the publishing destination to update.
newUpdatePublishingDestination ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'destinationId'
  Core.Text ->
  UpdatePublishingDestination
newUpdatePublishingDestination
  pDetectorId_
  pDestinationId_ =
    UpdatePublishingDestination'
      { destinationProperties =
          Core.Nothing,
        detectorId = pDetectorId_,
        destinationId = pDestinationId_
      }

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and
-- @KmsKeyArn@ of the publishing destination.
updatePublishingDestination_destinationProperties :: Lens.Lens' UpdatePublishingDestination (Core.Maybe DestinationProperties)
updatePublishingDestination_destinationProperties = Lens.lens (\UpdatePublishingDestination' {destinationProperties} -> destinationProperties) (\s@UpdatePublishingDestination' {} a -> s {destinationProperties = a} :: UpdatePublishingDestination)

-- | The ID of the detector associated with the publishing destinations to
-- update.
updatePublishingDestination_detectorId :: Lens.Lens' UpdatePublishingDestination Core.Text
updatePublishingDestination_detectorId = Lens.lens (\UpdatePublishingDestination' {detectorId} -> detectorId) (\s@UpdatePublishingDestination' {} a -> s {detectorId = a} :: UpdatePublishingDestination)

-- | The ID of the publishing destination to update.
updatePublishingDestination_destinationId :: Lens.Lens' UpdatePublishingDestination Core.Text
updatePublishingDestination_destinationId = Lens.lens (\UpdatePublishingDestination' {destinationId} -> destinationId) (\s@UpdatePublishingDestination' {} a -> s {destinationId = a} :: UpdatePublishingDestination)

instance Core.AWSRequest UpdatePublishingDestination where
  type
    AWSResponse UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePublishingDestination

instance Core.NFData UpdatePublishingDestination

instance Core.ToHeaders UpdatePublishingDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinationProperties" Core..=)
              Core.<$> destinationProperties
          ]
      )

instance Core.ToPath UpdatePublishingDestination where
  toPath UpdatePublishingDestination' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/publishingDestination/",
        Core.toBS destinationId
      ]

instance Core.ToQuery UpdatePublishingDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePublishingDestinationResponse' smart constructor.
data UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePublishingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePublishingDestinationResponse_httpStatus' - The response's http status code.
newUpdatePublishingDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePublishingDestinationResponse
newUpdatePublishingDestinationResponse pHttpStatus_ =
  UpdatePublishingDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePublishingDestinationResponse_httpStatus :: Lens.Lens' UpdatePublishingDestinationResponse Core.Int
updatePublishingDestinationResponse_httpStatus = Lens.lens (\UpdatePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdatePublishingDestinationResponse' {} a -> s {httpStatus = a} :: UpdatePublishingDestinationResponse)

instance
  Core.NFData
    UpdatePublishingDestinationResponse

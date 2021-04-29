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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePublishingDestination' smart constructor.
data UpdatePublishingDestination = UpdatePublishingDestination'
  { -- | A @DestinationProperties@ object that includes the @DestinationArn@ and
    -- @KmsKeyArn@ of the publishing destination.
    destinationProperties :: Prelude.Maybe DestinationProperties,
    -- | The ID of the detector associated with the publishing destinations to
    -- update.
    detectorId :: Prelude.Text,
    -- | The ID of the publishing destination to update.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'destinationId'
  Prelude.Text ->
  UpdatePublishingDestination
newUpdatePublishingDestination
  pDetectorId_
  pDestinationId_ =
    UpdatePublishingDestination'
      { destinationProperties =
          Prelude.Nothing,
        detectorId = pDetectorId_,
        destinationId = pDestinationId_
      }

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and
-- @KmsKeyArn@ of the publishing destination.
updatePublishingDestination_destinationProperties :: Lens.Lens' UpdatePublishingDestination (Prelude.Maybe DestinationProperties)
updatePublishingDestination_destinationProperties = Lens.lens (\UpdatePublishingDestination' {destinationProperties} -> destinationProperties) (\s@UpdatePublishingDestination' {} a -> s {destinationProperties = a} :: UpdatePublishingDestination)

-- | The ID of the detector associated with the publishing destinations to
-- update.
updatePublishingDestination_detectorId :: Lens.Lens' UpdatePublishingDestination Prelude.Text
updatePublishingDestination_detectorId = Lens.lens (\UpdatePublishingDestination' {detectorId} -> detectorId) (\s@UpdatePublishingDestination' {} a -> s {detectorId = a} :: UpdatePublishingDestination)

-- | The ID of the publishing destination to update.
updatePublishingDestination_destinationId :: Lens.Lens' UpdatePublishingDestination Prelude.Text
updatePublishingDestination_destinationId = Lens.lens (\UpdatePublishingDestination' {destinationId} -> destinationId) (\s@UpdatePublishingDestination' {} a -> s {destinationId = a} :: UpdatePublishingDestination)

instance
  Prelude.AWSRequest
    UpdatePublishingDestination
  where
  type
    Rs UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePublishingDestination

instance Prelude.NFData UpdatePublishingDestination

instance
  Prelude.ToHeaders
    UpdatePublishingDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("destinationProperties" Prelude..=)
              Prelude.<$> destinationProperties
          ]
      )

instance Prelude.ToPath UpdatePublishingDestination where
  toPath UpdatePublishingDestination' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/publishingDestination/",
        Prelude.toBS destinationId
      ]

instance Prelude.ToQuery UpdatePublishingDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePublishingDestinationResponse' smart constructor.
data UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdatePublishingDestinationResponse
newUpdatePublishingDestinationResponse pHttpStatus_ =
  UpdatePublishingDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePublishingDestinationResponse_httpStatus :: Lens.Lens' UpdatePublishingDestinationResponse Prelude.Int
updatePublishingDestinationResponse_httpStatus = Lens.lens (\UpdatePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdatePublishingDestinationResponse' {} a -> s {httpStatus = a} :: UpdatePublishingDestinationResponse)

instance
  Prelude.NFData
    UpdatePublishingDestinationResponse

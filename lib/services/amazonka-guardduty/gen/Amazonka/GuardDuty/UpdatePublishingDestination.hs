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
-- Module      : Amazonka.GuardDuty.UpdatePublishingDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the publishing destination specified by the
-- @destinationId@.
module Amazonka.GuardDuty.UpdatePublishingDestination
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UpdatePublishingDestination where
  type
    AWSResponse UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePublishingDestination where
  hashWithSalt _salt UpdatePublishingDestination' {..} =
    _salt
      `Prelude.hashWithSalt` destinationProperties
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` destinationId

instance Prelude.NFData UpdatePublishingDestination where
  rnf UpdatePublishingDestination' {..} =
    Prelude.rnf destinationProperties
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf destinationId

instance Data.ToHeaders UpdatePublishingDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinationProperties" Data..=)
              Prelude.<$> destinationProperties
          ]
      )

instance Data.ToPath UpdatePublishingDestination where
  toPath UpdatePublishingDestination' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/publishingDestination/",
        Data.toBS destinationId
      ]

instance Data.ToQuery UpdatePublishingDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePublishingDestinationResponse' smart constructor.
data UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdatePublishingDestinationResponse' {..} =
    Prelude.rnf httpStatus

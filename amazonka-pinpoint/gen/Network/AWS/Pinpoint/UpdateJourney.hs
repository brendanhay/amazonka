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
-- Module      : Network.AWS.Pinpoint.UpdateJourney
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a journey.
module Network.AWS.Pinpoint.UpdateJourney
  ( -- * Creating a Request
    UpdateJourney (..),
    newUpdateJourney,

    -- * Request Lenses
    updateJourney_journeyId,
    updateJourney_applicationId,
    updateJourney_writeJourneyRequest,

    -- * Destructuring the Response
    UpdateJourneyResponse (..),
    newUpdateJourneyResponse,

    -- * Response Lenses
    updateJourneyResponse_httpStatus,
    updateJourneyResponse_journeyResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateJourney' smart constructor.
data UpdateJourney = UpdateJourney'
  { -- | The unique identifier for the journey.
    journeyId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeJourneyRequest :: WriteJourneyRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJourney' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journeyId', 'updateJourney_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'updateJourney_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeJourneyRequest', 'updateJourney_writeJourneyRequest' - Undocumented member.
newUpdateJourney ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeJourneyRequest'
  WriteJourneyRequest ->
  UpdateJourney
newUpdateJourney
  pJourneyId_
  pApplicationId_
  pWriteJourneyRequest_ =
    UpdateJourney'
      { journeyId = pJourneyId_,
        applicationId = pApplicationId_,
        writeJourneyRequest = pWriteJourneyRequest_
      }

-- | The unique identifier for the journey.
updateJourney_journeyId :: Lens.Lens' UpdateJourney Prelude.Text
updateJourney_journeyId = Lens.lens (\UpdateJourney' {journeyId} -> journeyId) (\s@UpdateJourney' {} a -> s {journeyId = a} :: UpdateJourney)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateJourney_applicationId :: Lens.Lens' UpdateJourney Prelude.Text
updateJourney_applicationId = Lens.lens (\UpdateJourney' {applicationId} -> applicationId) (\s@UpdateJourney' {} a -> s {applicationId = a} :: UpdateJourney)

-- | Undocumented member.
updateJourney_writeJourneyRequest :: Lens.Lens' UpdateJourney WriteJourneyRequest
updateJourney_writeJourneyRequest = Lens.lens (\UpdateJourney' {writeJourneyRequest} -> writeJourneyRequest) (\s@UpdateJourney' {} a -> s {writeJourneyRequest = a} :: UpdateJourney)

instance Core.AWSRequest UpdateJourney where
  type
    AWSResponse UpdateJourney =
      UpdateJourneyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJourneyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateJourney

instance Prelude.NFData UpdateJourney

instance Core.ToHeaders UpdateJourney where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateJourney where
  toJSON UpdateJourney' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WriteJourneyRequest" Core..= writeJourneyRequest)
          ]
      )

instance Core.ToPath UpdateJourney where
  toPath UpdateJourney' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId
      ]

instance Core.ToQuery UpdateJourney where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJourneyResponse' smart constructor.
data UpdateJourneyResponse = UpdateJourneyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyResponse :: JourneyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJourneyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateJourneyResponse_httpStatus' - The response's http status code.
--
-- 'journeyResponse', 'updateJourneyResponse_journeyResponse' - Undocumented member.
newUpdateJourneyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  UpdateJourneyResponse
newUpdateJourneyResponse
  pHttpStatus_
  pJourneyResponse_ =
    UpdateJourneyResponse'
      { httpStatus = pHttpStatus_,
        journeyResponse = pJourneyResponse_
      }

-- | The response's http status code.
updateJourneyResponse_httpStatus :: Lens.Lens' UpdateJourneyResponse Prelude.Int
updateJourneyResponse_httpStatus = Lens.lens (\UpdateJourneyResponse' {httpStatus} -> httpStatus) (\s@UpdateJourneyResponse' {} a -> s {httpStatus = a} :: UpdateJourneyResponse)

-- | Undocumented member.
updateJourneyResponse_journeyResponse :: Lens.Lens' UpdateJourneyResponse JourneyResponse
updateJourneyResponse_journeyResponse = Lens.lens (\UpdateJourneyResponse' {journeyResponse} -> journeyResponse) (\s@UpdateJourneyResponse' {} a -> s {journeyResponse = a} :: UpdateJourneyResponse)

instance Prelude.NFData UpdateJourneyResponse

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
-- Module      : Amazonka.Pinpoint.UpdateJourneyState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) an active journey.
module Amazonka.Pinpoint.UpdateJourneyState
  ( -- * Creating a Request
    UpdateJourneyState (..),
    newUpdateJourneyState,

    -- * Request Lenses
    updateJourneyState_journeyId,
    updateJourneyState_applicationId,
    updateJourneyState_journeyStateRequest,

    -- * Destructuring the Response
    UpdateJourneyStateResponse (..),
    newUpdateJourneyStateResponse,

    -- * Response Lenses
    updateJourneyStateResponse_httpStatus,
    updateJourneyStateResponse_journeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateJourneyState' smart constructor.
data UpdateJourneyState = UpdateJourneyState'
  { -- | The unique identifier for the journey.
    journeyId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    journeyStateRequest :: JourneyStateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJourneyState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journeyId', 'updateJourneyState_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'updateJourneyState_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'journeyStateRequest', 'updateJourneyState_journeyStateRequest' - Undocumented member.
newUpdateJourneyState ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'journeyStateRequest'
  JourneyStateRequest ->
  UpdateJourneyState
newUpdateJourneyState
  pJourneyId_
  pApplicationId_
  pJourneyStateRequest_ =
    UpdateJourneyState'
      { journeyId = pJourneyId_,
        applicationId = pApplicationId_,
        journeyStateRequest = pJourneyStateRequest_
      }

-- | The unique identifier for the journey.
updateJourneyState_journeyId :: Lens.Lens' UpdateJourneyState Prelude.Text
updateJourneyState_journeyId = Lens.lens (\UpdateJourneyState' {journeyId} -> journeyId) (\s@UpdateJourneyState' {} a -> s {journeyId = a} :: UpdateJourneyState)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateJourneyState_applicationId :: Lens.Lens' UpdateJourneyState Prelude.Text
updateJourneyState_applicationId = Lens.lens (\UpdateJourneyState' {applicationId} -> applicationId) (\s@UpdateJourneyState' {} a -> s {applicationId = a} :: UpdateJourneyState)

-- | Undocumented member.
updateJourneyState_journeyStateRequest :: Lens.Lens' UpdateJourneyState JourneyStateRequest
updateJourneyState_journeyStateRequest = Lens.lens (\UpdateJourneyState' {journeyStateRequest} -> journeyStateRequest) (\s@UpdateJourneyState' {} a -> s {journeyStateRequest = a} :: UpdateJourneyState)

instance Core.AWSRequest UpdateJourneyState where
  type
    AWSResponse UpdateJourneyState =
      UpdateJourneyStateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJourneyStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateJourneyState where
  hashWithSalt _salt UpdateJourneyState' {..} =
    _salt `Prelude.hashWithSalt` journeyId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` journeyStateRequest

instance Prelude.NFData UpdateJourneyState where
  rnf UpdateJourneyState' {..} =
    Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf journeyStateRequest

instance Core.ToHeaders UpdateJourneyState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateJourneyState where
  toJSON UpdateJourneyState' {..} =
    Core.toJSON journeyStateRequest

instance Core.ToPath UpdateJourneyState where
  toPath UpdateJourneyState' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId,
        "/state"
      ]

instance Core.ToQuery UpdateJourneyState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJourneyStateResponse' smart constructor.
data UpdateJourneyStateResponse = UpdateJourneyStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyResponse :: JourneyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJourneyStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateJourneyStateResponse_httpStatus' - The response's http status code.
--
-- 'journeyResponse', 'updateJourneyStateResponse_journeyResponse' - Undocumented member.
newUpdateJourneyStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  UpdateJourneyStateResponse
newUpdateJourneyStateResponse
  pHttpStatus_
  pJourneyResponse_ =
    UpdateJourneyStateResponse'
      { httpStatus =
          pHttpStatus_,
        journeyResponse = pJourneyResponse_
      }

-- | The response's http status code.
updateJourneyStateResponse_httpStatus :: Lens.Lens' UpdateJourneyStateResponse Prelude.Int
updateJourneyStateResponse_httpStatus = Lens.lens (\UpdateJourneyStateResponse' {httpStatus} -> httpStatus) (\s@UpdateJourneyStateResponse' {} a -> s {httpStatus = a} :: UpdateJourneyStateResponse)

-- | Undocumented member.
updateJourneyStateResponse_journeyResponse :: Lens.Lens' UpdateJourneyStateResponse JourneyResponse
updateJourneyStateResponse_journeyResponse = Lens.lens (\UpdateJourneyStateResponse' {journeyResponse} -> journeyResponse) (\s@UpdateJourneyStateResponse' {} a -> s {journeyResponse = a} :: UpdateJourneyStateResponse)

instance Prelude.NFData UpdateJourneyStateResponse where
  rnf UpdateJourneyStateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyResponse

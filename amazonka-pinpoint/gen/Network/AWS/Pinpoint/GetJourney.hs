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
-- Module      : Network.AWS.Pinpoint.GetJourney
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for a journey.
module Network.AWS.Pinpoint.GetJourney
  ( -- * Creating a Request
    GetJourney (..),
    newGetJourney,

    -- * Request Lenses
    getJourney_journeyId,
    getJourney_applicationId,

    -- * Destructuring the Response
    GetJourneyResponse (..),
    newGetJourneyResponse,

    -- * Response Lenses
    getJourneyResponse_httpStatus,
    getJourneyResponse_journeyResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJourney' smart constructor.
data GetJourney = GetJourney'
  { -- | The unique identifier for the journey.
    journeyId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJourney' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journeyId', 'getJourney_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'getJourney_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetJourney ::
  -- | 'journeyId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetJourney
newGetJourney pJourneyId_ pApplicationId_ =
  GetJourney'
    { journeyId = pJourneyId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the journey.
getJourney_journeyId :: Lens.Lens' GetJourney Core.Text
getJourney_journeyId = Lens.lens (\GetJourney' {journeyId} -> journeyId) (\s@GetJourney' {} a -> s {journeyId = a} :: GetJourney)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourney_applicationId :: Lens.Lens' GetJourney Core.Text
getJourney_applicationId = Lens.lens (\GetJourney' {applicationId} -> applicationId) (\s@GetJourney' {} a -> s {applicationId = a} :: GetJourney)

instance Core.AWSRequest GetJourney where
  type AWSResponse GetJourney = GetJourneyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetJourney

instance Core.NFData GetJourney

instance Core.ToHeaders GetJourney where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetJourney where
  toPath GetJourney' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId
      ]

instance Core.ToQuery GetJourney where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJourneyResponse' smart constructor.
data GetJourneyResponse = GetJourneyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    journeyResponse :: JourneyResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJourneyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyResponse_httpStatus' - The response's http status code.
--
-- 'journeyResponse', 'getJourneyResponse_journeyResponse' - Undocumented member.
newGetJourneyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  GetJourneyResponse
newGetJourneyResponse pHttpStatus_ pJourneyResponse_ =
  GetJourneyResponse'
    { httpStatus = pHttpStatus_,
      journeyResponse = pJourneyResponse_
    }

-- | The response's http status code.
getJourneyResponse_httpStatus :: Lens.Lens' GetJourneyResponse Core.Int
getJourneyResponse_httpStatus = Lens.lens (\GetJourneyResponse' {httpStatus} -> httpStatus) (\s@GetJourneyResponse' {} a -> s {httpStatus = a} :: GetJourneyResponse)

-- | Undocumented member.
getJourneyResponse_journeyResponse :: Lens.Lens' GetJourneyResponse JourneyResponse
getJourneyResponse_journeyResponse = Lens.lens (\GetJourneyResponse' {journeyResponse} -> journeyResponse) (\s@GetJourneyResponse' {} a -> s {journeyResponse = a} :: GetJourneyResponse)

instance Core.NFData GetJourneyResponse

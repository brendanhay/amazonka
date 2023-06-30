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
-- Module      : Amazonka.Pinpoint.GetJourney
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for a journey.
module Amazonka.Pinpoint.GetJourney
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourney' smart constructor.
data GetJourney = GetJourney'
  { -- | The unique identifier for the journey.
    journeyId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  GetJourney
newGetJourney pJourneyId_ pApplicationId_ =
  GetJourney'
    { journeyId = pJourneyId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the journey.
getJourney_journeyId :: Lens.Lens' GetJourney Prelude.Text
getJourney_journeyId = Lens.lens (\GetJourney' {journeyId} -> journeyId) (\s@GetJourney' {} a -> s {journeyId = a} :: GetJourney)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourney_applicationId :: Lens.Lens' GetJourney Prelude.Text
getJourney_applicationId = Lens.lens (\GetJourney' {applicationId} -> applicationId) (\s@GetJourney' {} a -> s {applicationId = a} :: GetJourney)

instance Core.AWSRequest GetJourney where
  type AWSResponse GetJourney = GetJourneyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetJourney where
  hashWithSalt _salt GetJourney' {..} =
    _salt
      `Prelude.hashWithSalt` journeyId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetJourney where
  rnf GetJourney' {..} =
    Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders GetJourney where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJourney where
  toPath GetJourney' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/journeys/",
        Data.toBS journeyId
      ]

instance Data.ToQuery GetJourney where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJourneyResponse' smart constructor.
data GetJourneyResponse = GetJourneyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyResponse :: JourneyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  GetJourneyResponse
newGetJourneyResponse pHttpStatus_ pJourneyResponse_ =
  GetJourneyResponse'
    { httpStatus = pHttpStatus_,
      journeyResponse = pJourneyResponse_
    }

-- | The response's http status code.
getJourneyResponse_httpStatus :: Lens.Lens' GetJourneyResponse Prelude.Int
getJourneyResponse_httpStatus = Lens.lens (\GetJourneyResponse' {httpStatus} -> httpStatus) (\s@GetJourneyResponse' {} a -> s {httpStatus = a} :: GetJourneyResponse)

-- | Undocumented member.
getJourneyResponse_journeyResponse :: Lens.Lens' GetJourneyResponse JourneyResponse
getJourneyResponse_journeyResponse = Lens.lens (\GetJourneyResponse' {journeyResponse} -> journeyResponse) (\s@GetJourneyResponse' {} a -> s {journeyResponse = a} :: GetJourneyResponse)

instance Prelude.NFData GetJourneyResponse where
  rnf GetJourneyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyResponse

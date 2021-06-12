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
-- Module      : Network.AWS.Pinpoint.DeleteJourney
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a journey from an application.
module Network.AWS.Pinpoint.DeleteJourney
  ( -- * Creating a Request
    DeleteJourney (..),
    newDeleteJourney,

    -- * Request Lenses
    deleteJourney_journeyId,
    deleteJourney_applicationId,

    -- * Destructuring the Response
    DeleteJourneyResponse (..),
    newDeleteJourneyResponse,

    -- * Response Lenses
    deleteJourneyResponse_httpStatus,
    deleteJourneyResponse_journeyResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJourney' smart constructor.
data DeleteJourney = DeleteJourney'
  { -- | The unique identifier for the journey.
    journeyId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJourney' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journeyId', 'deleteJourney_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'deleteJourney_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteJourney ::
  -- | 'journeyId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  DeleteJourney
newDeleteJourney pJourneyId_ pApplicationId_ =
  DeleteJourney'
    { journeyId = pJourneyId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the journey.
deleteJourney_journeyId :: Lens.Lens' DeleteJourney Core.Text
deleteJourney_journeyId = Lens.lens (\DeleteJourney' {journeyId} -> journeyId) (\s@DeleteJourney' {} a -> s {journeyId = a} :: DeleteJourney)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteJourney_applicationId :: Lens.Lens' DeleteJourney Core.Text
deleteJourney_applicationId = Lens.lens (\DeleteJourney' {applicationId} -> applicationId) (\s@DeleteJourney' {} a -> s {applicationId = a} :: DeleteJourney)

instance Core.AWSRequest DeleteJourney where
  type
    AWSResponse DeleteJourney =
      DeleteJourneyResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJourneyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteJourney

instance Core.NFData DeleteJourney

instance Core.ToHeaders DeleteJourney where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteJourney where
  toPath DeleteJourney' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId
      ]

instance Core.ToQuery DeleteJourney where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteJourneyResponse' smart constructor.
data DeleteJourneyResponse = DeleteJourneyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    journeyResponse :: JourneyResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJourneyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteJourneyResponse_httpStatus' - The response's http status code.
--
-- 'journeyResponse', 'deleteJourneyResponse_journeyResponse' - Undocumented member.
newDeleteJourneyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'journeyResponse'
  JourneyResponse ->
  DeleteJourneyResponse
newDeleteJourneyResponse
  pHttpStatus_
  pJourneyResponse_ =
    DeleteJourneyResponse'
      { httpStatus = pHttpStatus_,
        journeyResponse = pJourneyResponse_
      }

-- | The response's http status code.
deleteJourneyResponse_httpStatus :: Lens.Lens' DeleteJourneyResponse Core.Int
deleteJourneyResponse_httpStatus = Lens.lens (\DeleteJourneyResponse' {httpStatus} -> httpStatus) (\s@DeleteJourneyResponse' {} a -> s {httpStatus = a} :: DeleteJourneyResponse)

-- | Undocumented member.
deleteJourneyResponse_journeyResponse :: Lens.Lens' DeleteJourneyResponse JourneyResponse
deleteJourneyResponse_journeyResponse = Lens.lens (\DeleteJourneyResponse' {journeyResponse} -> journeyResponse) (\s@DeleteJourneyResponse' {} a -> s {journeyResponse = a} :: DeleteJourneyResponse)

instance Core.NFData DeleteJourneyResponse

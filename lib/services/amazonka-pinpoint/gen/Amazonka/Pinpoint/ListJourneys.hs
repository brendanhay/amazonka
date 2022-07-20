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
-- Module      : Amazonka.Pinpoint.ListJourneys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for all the journeys that are associated with an application.
module Amazonka.Pinpoint.ListJourneys
  ( -- * Creating a Request
    ListJourneys (..),
    newListJourneys,

    -- * Request Lenses
    listJourneys_pageSize,
    listJourneys_token,
    listJourneys_applicationId,

    -- * Destructuring the Response
    ListJourneysResponse (..),
    newListJourneysResponse,

    -- * Response Lenses
    listJourneysResponse_httpStatus,
    listJourneysResponse_journeysResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJourneys' smart constructor.
data ListJourneys = ListJourneys'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJourneys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listJourneys_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'listJourneys_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'listJourneys_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newListJourneys ::
  -- | 'applicationId'
  Prelude.Text ->
  ListJourneys
newListJourneys pApplicationId_ =
  ListJourneys'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listJourneys_pageSize :: Lens.Lens' ListJourneys (Prelude.Maybe Prelude.Text)
listJourneys_pageSize = Lens.lens (\ListJourneys' {pageSize} -> pageSize) (\s@ListJourneys' {} a -> s {pageSize = a} :: ListJourneys)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
listJourneys_token :: Lens.Lens' ListJourneys (Prelude.Maybe Prelude.Text)
listJourneys_token = Lens.lens (\ListJourneys' {token} -> token) (\s@ListJourneys' {} a -> s {token = a} :: ListJourneys)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
listJourneys_applicationId :: Lens.Lens' ListJourneys Prelude.Text
listJourneys_applicationId = Lens.lens (\ListJourneys' {applicationId} -> applicationId) (\s@ListJourneys' {} a -> s {applicationId = a} :: ListJourneys)

instance Core.AWSRequest ListJourneys where
  type AWSResponse ListJourneys = ListJourneysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJourneysResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable ListJourneys where
  hashWithSalt _salt ListJourneys' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListJourneys where
  rnf ListJourneys' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders ListJourneys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListJourneys where
  toPath ListJourneys' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/journeys"]

instance Core.ToQuery ListJourneys where
  toQuery ListJourneys' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newListJourneysResponse' smart constructor.
data ListJourneysResponse = ListJourneysResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeysResponse :: JourneysResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJourneysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listJourneysResponse_httpStatus' - The response's http status code.
--
-- 'journeysResponse', 'listJourneysResponse_journeysResponse' - Undocumented member.
newListJourneysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeysResponse'
  JourneysResponse ->
  ListJourneysResponse
newListJourneysResponse
  pHttpStatus_
  pJourneysResponse_ =
    ListJourneysResponse'
      { httpStatus = pHttpStatus_,
        journeysResponse = pJourneysResponse_
      }

-- | The response's http status code.
listJourneysResponse_httpStatus :: Lens.Lens' ListJourneysResponse Prelude.Int
listJourneysResponse_httpStatus = Lens.lens (\ListJourneysResponse' {httpStatus} -> httpStatus) (\s@ListJourneysResponse' {} a -> s {httpStatus = a} :: ListJourneysResponse)

-- | Undocumented member.
listJourneysResponse_journeysResponse :: Lens.Lens' ListJourneysResponse JourneysResponse
listJourneysResponse_journeysResponse = Lens.lens (\ListJourneysResponse' {journeysResponse} -> journeysResponse) (\s@ListJourneysResponse' {} a -> s {journeysResponse = a} :: ListJourneysResponse)

instance Prelude.NFData ListJourneysResponse where
  rnf ListJourneysResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeysResponse

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
-- Module      : Amazonka.WorkMail.ListAvailabilityConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the @AvailabilityConfiguration@\'s for the given WorkMail
-- organization.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListAvailabilityConfigurations
  ( -- * Creating a Request
    ListAvailabilityConfigurations (..),
    newListAvailabilityConfigurations,

    -- * Request Lenses
    listAvailabilityConfigurations_maxResults,
    listAvailabilityConfigurations_nextToken,
    listAvailabilityConfigurations_organizationId,

    -- * Destructuring the Response
    ListAvailabilityConfigurationsResponse (..),
    newListAvailabilityConfigurationsResponse,

    -- * Response Lenses
    listAvailabilityConfigurationsResponse_availabilityConfigurations,
    listAvailabilityConfigurationsResponse_nextToken,
    listAvailabilityConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListAvailabilityConfigurations' smart constructor.
data ListAvailabilityConfigurations = ListAvailabilityConfigurations'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not require a token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail organization for which the @AvailabilityConfiguration@\'s
    -- will be listed.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailabilityConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAvailabilityConfigurations_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listAvailabilityConfigurations_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not require a token.
--
-- 'organizationId', 'listAvailabilityConfigurations_organizationId' - The WorkMail organization for which the @AvailabilityConfiguration@\'s
-- will be listed.
newListAvailabilityConfigurations ::
  -- | 'organizationId'
  Prelude.Text ->
  ListAvailabilityConfigurations
newListAvailabilityConfigurations pOrganizationId_ =
  ListAvailabilityConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The maximum number of results to return in a single call.
listAvailabilityConfigurations_maxResults :: Lens.Lens' ListAvailabilityConfigurations (Prelude.Maybe Prelude.Natural)
listAvailabilityConfigurations_maxResults = Lens.lens (\ListAvailabilityConfigurations' {maxResults} -> maxResults) (\s@ListAvailabilityConfigurations' {} a -> s {maxResults = a} :: ListAvailabilityConfigurations)

-- | The token to use to retrieve the next page of results. The first call
-- does not require a token.
listAvailabilityConfigurations_nextToken :: Lens.Lens' ListAvailabilityConfigurations (Prelude.Maybe Prelude.Text)
listAvailabilityConfigurations_nextToken = Lens.lens (\ListAvailabilityConfigurations' {nextToken} -> nextToken) (\s@ListAvailabilityConfigurations' {} a -> s {nextToken = a} :: ListAvailabilityConfigurations)

-- | The WorkMail organization for which the @AvailabilityConfiguration@\'s
-- will be listed.
listAvailabilityConfigurations_organizationId :: Lens.Lens' ListAvailabilityConfigurations Prelude.Text
listAvailabilityConfigurations_organizationId = Lens.lens (\ListAvailabilityConfigurations' {organizationId} -> organizationId) (\s@ListAvailabilityConfigurations' {} a -> s {organizationId = a} :: ListAvailabilityConfigurations)

instance Core.AWSPager ListAvailabilityConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAvailabilityConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAvailabilityConfigurationsResponse_availabilityConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAvailabilityConfigurations_nextToken
          Lens..~ rs
          Lens.^? listAvailabilityConfigurationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAvailabilityConfigurations
  where
  type
    AWSResponse ListAvailabilityConfigurations =
      ListAvailabilityConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailabilityConfigurationsResponse'
            Prelude.<$> ( x
                            Data..?> "AvailabilityConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailabilityConfigurations
  where
  hashWithSalt
    _salt
    ListAvailabilityConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationId

instance
  Prelude.NFData
    ListAvailabilityConfigurations
  where
  rnf ListAvailabilityConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationId

instance
  Data.ToHeaders
    ListAvailabilityConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListAvailabilityConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAvailabilityConfigurations where
  toJSON ListAvailabilityConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath ListAvailabilityConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAvailabilityConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailabilityConfigurationsResponse' smart constructor.
data ListAvailabilityConfigurationsResponse = ListAvailabilityConfigurationsResponse'
  { -- | The list of @AvailabilityConfiguration@\'s that exist for the specified
    -- WorkMail organization.
    availabilityConfigurations :: Prelude.Maybe [AvailabilityConfiguration],
    -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no further results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailabilityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityConfigurations', 'listAvailabilityConfigurationsResponse_availabilityConfigurations' - The list of @AvailabilityConfiguration@\'s that exist for the specified
-- WorkMail organization.
--
-- 'nextToken', 'listAvailabilityConfigurationsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no further results to return.
--
-- 'httpStatus', 'listAvailabilityConfigurationsResponse_httpStatus' - The response's http status code.
newListAvailabilityConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailabilityConfigurationsResponse
newListAvailabilityConfigurationsResponse
  pHttpStatus_ =
    ListAvailabilityConfigurationsResponse'
      { availabilityConfigurations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of @AvailabilityConfiguration@\'s that exist for the specified
-- WorkMail organization.
listAvailabilityConfigurationsResponse_availabilityConfigurations :: Lens.Lens' ListAvailabilityConfigurationsResponse (Prelude.Maybe [AvailabilityConfiguration])
listAvailabilityConfigurationsResponse_availabilityConfigurations = Lens.lens (\ListAvailabilityConfigurationsResponse' {availabilityConfigurations} -> availabilityConfigurations) (\s@ListAvailabilityConfigurationsResponse' {} a -> s {availabilityConfigurations = a} :: ListAvailabilityConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no further results to return.
listAvailabilityConfigurationsResponse_nextToken :: Lens.Lens' ListAvailabilityConfigurationsResponse (Prelude.Maybe Prelude.Text)
listAvailabilityConfigurationsResponse_nextToken = Lens.lens (\ListAvailabilityConfigurationsResponse' {nextToken} -> nextToken) (\s@ListAvailabilityConfigurationsResponse' {} a -> s {nextToken = a} :: ListAvailabilityConfigurationsResponse)

-- | The response's http status code.
listAvailabilityConfigurationsResponse_httpStatus :: Lens.Lens' ListAvailabilityConfigurationsResponse Prelude.Int
listAvailabilityConfigurationsResponse_httpStatus = Lens.lens (\ListAvailabilityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListAvailabilityConfigurationsResponse' {} a -> s {httpStatus = a} :: ListAvailabilityConfigurationsResponse)

instance
  Prelude.NFData
    ListAvailabilityConfigurationsResponse
  where
  rnf ListAvailabilityConfigurationsResponse' {..} =
    Prelude.rnf availabilityConfigurations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

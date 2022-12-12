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
-- Module      : Amazonka.FraudDetector.GetEventTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all event types or a specific event type if name is provided. This
-- is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 10 records per page. If you provide a
-- @maxResults@, the value must be between 5 and 10. To get the next page
-- results, provide the pagination token from the @GetEventTypesResponse@
-- as part of your request. A null pagination token fetches the records
-- from the beginning.
module Amazonka.FraudDetector.GetEventTypes
  ( -- * Creating a Request
    GetEventTypes (..),
    newGetEventTypes,

    -- * Request Lenses
    getEventTypes_maxResults,
    getEventTypes_name,
    getEventTypes_nextToken,

    -- * Destructuring the Response
    GetEventTypesResponse (..),
    newGetEventTypesResponse,

    -- * Response Lenses
    getEventTypesResponse_eventTypes,
    getEventTypesResponse_nextToken,
    getEventTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventTypes' smart constructor.
data GetEventTypes = GetEventTypes'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getEventTypes_maxResults' - The maximum number of objects to return for the request.
--
-- 'name', 'getEventTypes_name' - The name.
--
-- 'nextToken', 'getEventTypes_nextToken' - The next token for the subsequent request.
newGetEventTypes ::
  GetEventTypes
newGetEventTypes =
  GetEventTypes'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects to return for the request.
getEventTypes_maxResults :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Natural)
getEventTypes_maxResults = Lens.lens (\GetEventTypes' {maxResults} -> maxResults) (\s@GetEventTypes' {} a -> s {maxResults = a} :: GetEventTypes)

-- | The name.
getEventTypes_name :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Text)
getEventTypes_name = Lens.lens (\GetEventTypes' {name} -> name) (\s@GetEventTypes' {} a -> s {name = a} :: GetEventTypes)

-- | The next token for the subsequent request.
getEventTypes_nextToken :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Text)
getEventTypes_nextToken = Lens.lens (\GetEventTypes' {nextToken} -> nextToken) (\s@GetEventTypes' {} a -> s {nextToken = a} :: GetEventTypes)

instance Core.AWSRequest GetEventTypes where
  type
    AWSResponse GetEventTypes =
      GetEventTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventTypesResponse'
            Prelude.<$> (x Data..?> "eventTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventTypes where
  hashWithSalt _salt GetEventTypes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetEventTypes where
  rnf GetEventTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetEventTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetEventTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEventTypes where
  toJSON GetEventTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetEventTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEventTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventTypesResponse' smart constructor.
data GetEventTypesResponse = GetEventTypesResponse'
  { -- | An array of event types.
    eventTypes :: Prelude.Maybe [Data.Sensitive EventType],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypes', 'getEventTypesResponse_eventTypes' - An array of event types.
--
-- 'nextToken', 'getEventTypesResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getEventTypesResponse_httpStatus' - The response's http status code.
newGetEventTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventTypesResponse
newGetEventTypesResponse pHttpStatus_ =
  GetEventTypesResponse'
    { eventTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of event types.
getEventTypesResponse_eventTypes :: Lens.Lens' GetEventTypesResponse (Prelude.Maybe [EventType])
getEventTypesResponse_eventTypes = Lens.lens (\GetEventTypesResponse' {eventTypes} -> eventTypes) (\s@GetEventTypesResponse' {} a -> s {eventTypes = a} :: GetEventTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getEventTypesResponse_nextToken :: Lens.Lens' GetEventTypesResponse (Prelude.Maybe Prelude.Text)
getEventTypesResponse_nextToken = Lens.lens (\GetEventTypesResponse' {nextToken} -> nextToken) (\s@GetEventTypesResponse' {} a -> s {nextToken = a} :: GetEventTypesResponse)

-- | The response's http status code.
getEventTypesResponse_httpStatus :: Lens.Lens' GetEventTypesResponse Prelude.Int
getEventTypesResponse_httpStatus = Lens.lens (\GetEventTypesResponse' {httpStatus} -> httpStatus) (\s@GetEventTypesResponse' {} a -> s {httpStatus = a} :: GetEventTypesResponse)

instance Prelude.NFData GetEventTypesResponse where
  rnf GetEventTypesResponse' {..} =
    Prelude.rnf eventTypes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

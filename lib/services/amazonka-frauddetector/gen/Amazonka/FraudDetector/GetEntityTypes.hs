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
-- Module      : Amazonka.FraudDetector.GetEntityTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all entity types or a specific entity type if a name is specified.
-- This is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 10 records per page. If you provide a
-- @maxResults@, the value must be between 5 and 10. To get the next page
-- results, provide the pagination token from the @GetEntityTypesResponse@
-- as part of your request. A null pagination token fetches the records
-- from the beginning.
module Amazonka.FraudDetector.GetEntityTypes
  ( -- * Creating a Request
    GetEntityTypes (..),
    newGetEntityTypes,

    -- * Request Lenses
    getEntityTypes_maxResults,
    getEntityTypes_name,
    getEntityTypes_nextToken,

    -- * Destructuring the Response
    GetEntityTypesResponse (..),
    newGetEntityTypesResponse,

    -- * Response Lenses
    getEntityTypesResponse_entityTypes,
    getEntityTypesResponse_nextToken,
    getEntityTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEntityTypes' smart constructor.
data GetEntityTypes = GetEntityTypes'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntityTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getEntityTypes_maxResults' - The maximum number of objects to return for the request.
--
-- 'name', 'getEntityTypes_name' - The name.
--
-- 'nextToken', 'getEntityTypes_nextToken' - The next token for the subsequent request.
newGetEntityTypes ::
  GetEntityTypes
newGetEntityTypes =
  GetEntityTypes'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects to return for the request.
getEntityTypes_maxResults :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Natural)
getEntityTypes_maxResults = Lens.lens (\GetEntityTypes' {maxResults} -> maxResults) (\s@GetEntityTypes' {} a -> s {maxResults = a} :: GetEntityTypes)

-- | The name.
getEntityTypes_name :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Text)
getEntityTypes_name = Lens.lens (\GetEntityTypes' {name} -> name) (\s@GetEntityTypes' {} a -> s {name = a} :: GetEntityTypes)

-- | The next token for the subsequent request.
getEntityTypes_nextToken :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Text)
getEntityTypes_nextToken = Lens.lens (\GetEntityTypes' {nextToken} -> nextToken) (\s@GetEntityTypes' {} a -> s {nextToken = a} :: GetEntityTypes)

instance Core.AWSRequest GetEntityTypes where
  type
    AWSResponse GetEntityTypes =
      GetEntityTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntityTypesResponse'
            Prelude.<$> (x Data..?> "entityTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEntityTypes where
  hashWithSalt _salt GetEntityTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetEntityTypes where
  rnf GetEntityTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetEntityTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetEntityTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEntityTypes where
  toJSON GetEntityTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetEntityTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEntityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEntityTypesResponse' smart constructor.
data GetEntityTypesResponse = GetEntityTypesResponse'
  { -- | An array of entity types.
    entityTypes :: Prelude.Maybe [EntityType],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntityTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityTypes', 'getEntityTypesResponse_entityTypes' - An array of entity types.
--
-- 'nextToken', 'getEntityTypesResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getEntityTypesResponse_httpStatus' - The response's http status code.
newGetEntityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEntityTypesResponse
newGetEntityTypesResponse pHttpStatus_ =
  GetEntityTypesResponse'
    { entityTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of entity types.
getEntityTypesResponse_entityTypes :: Lens.Lens' GetEntityTypesResponse (Prelude.Maybe [EntityType])
getEntityTypesResponse_entityTypes = Lens.lens (\GetEntityTypesResponse' {entityTypes} -> entityTypes) (\s@GetEntityTypesResponse' {} a -> s {entityTypes = a} :: GetEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getEntityTypesResponse_nextToken :: Lens.Lens' GetEntityTypesResponse (Prelude.Maybe Prelude.Text)
getEntityTypesResponse_nextToken = Lens.lens (\GetEntityTypesResponse' {nextToken} -> nextToken) (\s@GetEntityTypesResponse' {} a -> s {nextToken = a} :: GetEntityTypesResponse)

-- | The response's http status code.
getEntityTypesResponse_httpStatus :: Lens.Lens' GetEntityTypesResponse Prelude.Int
getEntityTypesResponse_httpStatus = Lens.lens (\GetEntityTypesResponse' {httpStatus} -> httpStatus) (\s@GetEntityTypesResponse' {} a -> s {httpStatus = a} :: GetEntityTypesResponse)

instance Prelude.NFData GetEntityTypesResponse where
  rnf GetEntityTypesResponse' {..} =
    Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

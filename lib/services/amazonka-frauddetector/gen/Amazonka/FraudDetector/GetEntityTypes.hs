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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getEntityTypes_name,
    getEntityTypes_nextToken,
    getEntityTypes_maxResults,

    -- * Destructuring the Response
    GetEntityTypesResponse (..),
    newGetEntityTypesResponse,

    -- * Response Lenses
    getEntityTypesResponse_nextToken,
    getEntityTypesResponse_entityTypes,
    getEntityTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEntityTypes' smart constructor.
data GetEntityTypes = GetEntityTypes'
  { -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'name', 'getEntityTypes_name' - The name.
--
-- 'nextToken', 'getEntityTypes_nextToken' - The next token for the subsequent request.
--
-- 'maxResults', 'getEntityTypes_maxResults' - The maximum number of objects to return for the request.
newGetEntityTypes ::
  GetEntityTypes
newGetEntityTypes =
  GetEntityTypes'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name.
getEntityTypes_name :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Text)
getEntityTypes_name = Lens.lens (\GetEntityTypes' {name} -> name) (\s@GetEntityTypes' {} a -> s {name = a} :: GetEntityTypes)

-- | The next token for the subsequent request.
getEntityTypes_nextToken :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Text)
getEntityTypes_nextToken = Lens.lens (\GetEntityTypes' {nextToken} -> nextToken) (\s@GetEntityTypes' {} a -> s {nextToken = a} :: GetEntityTypes)

-- | The maximum number of objects to return for the request.
getEntityTypes_maxResults :: Lens.Lens' GetEntityTypes (Prelude.Maybe Prelude.Natural)
getEntityTypes_maxResults = Lens.lens (\GetEntityTypes' {maxResults} -> maxResults) (\s@GetEntityTypes' {} a -> s {maxResults = a} :: GetEntityTypes)

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
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "entityTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEntityTypes where
  hashWithSalt _salt GetEntityTypes' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetEntityTypes where
  rnf GetEntityTypes' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetEntityTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetEntityTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEntityTypes where
  toJSON GetEntityTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetEntityTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEntityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEntityTypesResponse' smart constructor.
data GetEntityTypesResponse = GetEntityTypesResponse'
  { -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of entity types.
    entityTypes :: Prelude.Maybe [EntityType],
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
-- 'nextToken', 'getEntityTypesResponse_nextToken' - The next page token.
--
-- 'entityTypes', 'getEntityTypesResponse_entityTypes' - An array of entity types.
--
-- 'httpStatus', 'getEntityTypesResponse_httpStatus' - The response's http status code.
newGetEntityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEntityTypesResponse
newGetEntityTypesResponse pHttpStatus_ =
  GetEntityTypesResponse'
    { nextToken =
        Prelude.Nothing,
      entityTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next page token.
getEntityTypesResponse_nextToken :: Lens.Lens' GetEntityTypesResponse (Prelude.Maybe Prelude.Text)
getEntityTypesResponse_nextToken = Lens.lens (\GetEntityTypesResponse' {nextToken} -> nextToken) (\s@GetEntityTypesResponse' {} a -> s {nextToken = a} :: GetEntityTypesResponse)

-- | An array of entity types.
getEntityTypesResponse_entityTypes :: Lens.Lens' GetEntityTypesResponse (Prelude.Maybe [EntityType])
getEntityTypesResponse_entityTypes = Lens.lens (\GetEntityTypesResponse' {entityTypes} -> entityTypes) (\s@GetEntityTypesResponse' {} a -> s {entityTypes = a} :: GetEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEntityTypesResponse_httpStatus :: Lens.Lens' GetEntityTypesResponse Prelude.Int
getEntityTypesResponse_httpStatus = Lens.lens (\GetEntityTypesResponse' {httpStatus} -> httpStatus) (\s@GetEntityTypesResponse' {} a -> s {httpStatus = a} :: GetEntityTypesResponse)

instance Prelude.NFData GetEntityTypesResponse where
  rnf GetEntityTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf httpStatus

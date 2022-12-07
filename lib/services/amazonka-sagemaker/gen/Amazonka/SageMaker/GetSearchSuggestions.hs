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
-- Module      : Amazonka.SageMaker.GetSearchSuggestions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An auto-complete API for the search functionality in the Amazon
-- SageMaker console. It returns suggestions of possible matches for the
-- property name to use in @Search@ queries. Provides suggestions for
-- @HyperParameters@, @Tags@, and @Metrics@.
module Amazonka.SageMaker.GetSearchSuggestions
  ( -- * Creating a Request
    GetSearchSuggestions (..),
    newGetSearchSuggestions,

    -- * Request Lenses
    getSearchSuggestions_suggestionQuery,
    getSearchSuggestions_resource,

    -- * Destructuring the Response
    GetSearchSuggestionsResponse (..),
    newGetSearchSuggestionsResponse,

    -- * Response Lenses
    getSearchSuggestionsResponse_propertyNameSuggestions,
    getSearchSuggestionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newGetSearchSuggestions' smart constructor.
data GetSearchSuggestions = GetSearchSuggestions'
  { -- | Limits the property names that are included in the response.
    suggestionQuery :: Prelude.Maybe SuggestionQuery,
    -- | The name of the Amazon SageMaker resource to search for.
    resource :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSearchSuggestions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestionQuery', 'getSearchSuggestions_suggestionQuery' - Limits the property names that are included in the response.
--
-- 'resource', 'getSearchSuggestions_resource' - The name of the Amazon SageMaker resource to search for.
newGetSearchSuggestions ::
  -- | 'resource'
  ResourceType ->
  GetSearchSuggestions
newGetSearchSuggestions pResource_ =
  GetSearchSuggestions'
    { suggestionQuery =
        Prelude.Nothing,
      resource = pResource_
    }

-- | Limits the property names that are included in the response.
getSearchSuggestions_suggestionQuery :: Lens.Lens' GetSearchSuggestions (Prelude.Maybe SuggestionQuery)
getSearchSuggestions_suggestionQuery = Lens.lens (\GetSearchSuggestions' {suggestionQuery} -> suggestionQuery) (\s@GetSearchSuggestions' {} a -> s {suggestionQuery = a} :: GetSearchSuggestions)

-- | The name of the Amazon SageMaker resource to search for.
getSearchSuggestions_resource :: Lens.Lens' GetSearchSuggestions ResourceType
getSearchSuggestions_resource = Lens.lens (\GetSearchSuggestions' {resource} -> resource) (\s@GetSearchSuggestions' {} a -> s {resource = a} :: GetSearchSuggestions)

instance Core.AWSRequest GetSearchSuggestions where
  type
    AWSResponse GetSearchSuggestions =
      GetSearchSuggestionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSearchSuggestionsResponse'
            Prelude.<$> ( x Data..?> "PropertyNameSuggestions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSearchSuggestions where
  hashWithSalt _salt GetSearchSuggestions' {..} =
    _salt `Prelude.hashWithSalt` suggestionQuery
      `Prelude.hashWithSalt` resource

instance Prelude.NFData GetSearchSuggestions where
  rnf GetSearchSuggestions' {..} =
    Prelude.rnf suggestionQuery
      `Prelude.seq` Prelude.rnf resource

instance Data.ToHeaders GetSearchSuggestions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.GetSearchSuggestions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSearchSuggestions where
  toJSON GetSearchSuggestions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SuggestionQuery" Data..=)
              Prelude.<$> suggestionQuery,
            Prelude.Just ("Resource" Data..= resource)
          ]
      )

instance Data.ToPath GetSearchSuggestions where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSearchSuggestions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSearchSuggestionsResponse' smart constructor.
data GetSearchSuggestionsResponse = GetSearchSuggestionsResponse'
  { -- | A list of property names for a @Resource@ that match a
    -- @SuggestionQuery@.
    propertyNameSuggestions :: Prelude.Maybe [PropertyNameSuggestion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSearchSuggestionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyNameSuggestions', 'getSearchSuggestionsResponse_propertyNameSuggestions' - A list of property names for a @Resource@ that match a
-- @SuggestionQuery@.
--
-- 'httpStatus', 'getSearchSuggestionsResponse_httpStatus' - The response's http status code.
newGetSearchSuggestionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSearchSuggestionsResponse
newGetSearchSuggestionsResponse pHttpStatus_ =
  GetSearchSuggestionsResponse'
    { propertyNameSuggestions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of property names for a @Resource@ that match a
-- @SuggestionQuery@.
getSearchSuggestionsResponse_propertyNameSuggestions :: Lens.Lens' GetSearchSuggestionsResponse (Prelude.Maybe [PropertyNameSuggestion])
getSearchSuggestionsResponse_propertyNameSuggestions = Lens.lens (\GetSearchSuggestionsResponse' {propertyNameSuggestions} -> propertyNameSuggestions) (\s@GetSearchSuggestionsResponse' {} a -> s {propertyNameSuggestions = a} :: GetSearchSuggestionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSearchSuggestionsResponse_httpStatus :: Lens.Lens' GetSearchSuggestionsResponse Prelude.Int
getSearchSuggestionsResponse_httpStatus = Lens.lens (\GetSearchSuggestionsResponse' {httpStatus} -> httpStatus) (\s@GetSearchSuggestionsResponse' {} a -> s {httpStatus = a} :: GetSearchSuggestionsResponse)

instance Prelude.NFData GetSearchSuggestionsResponse where
  rnf GetSearchSuggestionsResponse' {..} =
    Prelude.rnf propertyNameSuggestions
      `Prelude.seq` Prelude.rnf httpStatus

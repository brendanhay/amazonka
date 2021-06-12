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
-- Module      : Network.AWS.SageMaker.GetSearchSuggestions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An auto-complete API for the search functionality in the Amazon
-- SageMaker console. It returns suggestions of possible matches for the
-- property name to use in @Search@ queries. Provides suggestions for
-- @HyperParameters@, @Tags@, and @Metrics@.
module Network.AWS.SageMaker.GetSearchSuggestions
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newGetSearchSuggestions' smart constructor.
data GetSearchSuggestions = GetSearchSuggestions'
  { -- | Limits the property names that are included in the response.
    suggestionQuery :: Core.Maybe SuggestionQuery,
    -- | The name of the Amazon SageMaker resource to search for.
    resource :: ResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      resource = pResource_
    }

-- | Limits the property names that are included in the response.
getSearchSuggestions_suggestionQuery :: Lens.Lens' GetSearchSuggestions (Core.Maybe SuggestionQuery)
getSearchSuggestions_suggestionQuery = Lens.lens (\GetSearchSuggestions' {suggestionQuery} -> suggestionQuery) (\s@GetSearchSuggestions' {} a -> s {suggestionQuery = a} :: GetSearchSuggestions)

-- | The name of the Amazon SageMaker resource to search for.
getSearchSuggestions_resource :: Lens.Lens' GetSearchSuggestions ResourceType
getSearchSuggestions_resource = Lens.lens (\GetSearchSuggestions' {resource} -> resource) (\s@GetSearchSuggestions' {} a -> s {resource = a} :: GetSearchSuggestions)

instance Core.AWSRequest GetSearchSuggestions where
  type
    AWSResponse GetSearchSuggestions =
      GetSearchSuggestionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSearchSuggestionsResponse'
            Core.<$> ( x Core..?> "PropertyNameSuggestions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSearchSuggestions

instance Core.NFData GetSearchSuggestions

instance Core.ToHeaders GetSearchSuggestions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.GetSearchSuggestions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSearchSuggestions where
  toJSON GetSearchSuggestions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SuggestionQuery" Core..=)
              Core.<$> suggestionQuery,
            Core.Just ("Resource" Core..= resource)
          ]
      )

instance Core.ToPath GetSearchSuggestions where
  toPath = Core.const "/"

instance Core.ToQuery GetSearchSuggestions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSearchSuggestionsResponse' smart constructor.
data GetSearchSuggestionsResponse = GetSearchSuggestionsResponse'
  { -- | A list of property names for a @Resource@ that match a
    -- @SuggestionQuery@.
    propertyNameSuggestions :: Core.Maybe [PropertyNameSuggestion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetSearchSuggestionsResponse
newGetSearchSuggestionsResponse pHttpStatus_ =
  GetSearchSuggestionsResponse'
    { propertyNameSuggestions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of property names for a @Resource@ that match a
-- @SuggestionQuery@.
getSearchSuggestionsResponse_propertyNameSuggestions :: Lens.Lens' GetSearchSuggestionsResponse (Core.Maybe [PropertyNameSuggestion])
getSearchSuggestionsResponse_propertyNameSuggestions = Lens.lens (\GetSearchSuggestionsResponse' {propertyNameSuggestions} -> propertyNameSuggestions) (\s@GetSearchSuggestionsResponse' {} a -> s {propertyNameSuggestions = a} :: GetSearchSuggestionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSearchSuggestionsResponse_httpStatus :: Lens.Lens' GetSearchSuggestionsResponse Core.Int
getSearchSuggestionsResponse_httpStatus = Lens.lens (\GetSearchSuggestionsResponse' {httpStatus} -> httpStatus) (\s@GetSearchSuggestionsResponse' {} a -> s {httpStatus = a} :: GetSearchSuggestionsResponse)

instance Core.NFData GetSearchSuggestionsResponse

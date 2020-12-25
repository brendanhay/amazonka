{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.GetSearchSuggestions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An auto-complete API for the search functionality in the Amazon SageMaker console. It returns suggestions of possible matches for the property name to use in @Search@ queries. Provides suggestions for @HyperParameters@ , @Tags@ , and @Metrics@ .
module Network.AWS.SageMaker.GetSearchSuggestions
  ( -- * Creating a request
    GetSearchSuggestions (..),
    mkGetSearchSuggestions,

    -- ** Request lenses
    gssResource,
    gssSuggestionQuery,

    -- * Destructuring the response
    GetSearchSuggestionsResponse (..),
    mkGetSearchSuggestionsResponse,

    -- ** Response lenses
    gssrrsPropertyNameSuggestions,
    gssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkGetSearchSuggestions' smart constructor.
data GetSearchSuggestions = GetSearchSuggestions'
  { -- | The name of the Amazon SageMaker resource to search for.
    resource :: Types.ResourceType,
    -- | Limits the property names that are included in the response.
    suggestionQuery :: Core.Maybe Types.SuggestionQuery
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSearchSuggestions' value with any optional fields omitted.
mkGetSearchSuggestions ::
  -- | 'resource'
  Types.ResourceType ->
  GetSearchSuggestions
mkGetSearchSuggestions resource =
  GetSearchSuggestions' {resource, suggestionQuery = Core.Nothing}

-- | The name of the Amazon SageMaker resource to search for.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssResource :: Lens.Lens' GetSearchSuggestions Types.ResourceType
gssResource = Lens.field @"resource"
{-# DEPRECATED gssResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | Limits the property names that are included in the response.
--
-- /Note:/ Consider using 'suggestionQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssSuggestionQuery :: Lens.Lens' GetSearchSuggestions (Core.Maybe Types.SuggestionQuery)
gssSuggestionQuery = Lens.field @"suggestionQuery"
{-# DEPRECATED gssSuggestionQuery "Use generic-lens or generic-optics with 'suggestionQuery' instead." #-}

instance Core.FromJSON GetSearchSuggestions where
  toJSON GetSearchSuggestions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Resource" Core..= resource),
            ("SuggestionQuery" Core..=) Core.<$> suggestionQuery
          ]
      )

instance Core.AWSRequest GetSearchSuggestions where
  type Rs GetSearchSuggestions = GetSearchSuggestionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.GetSearchSuggestions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSearchSuggestionsResponse'
            Core.<$> (x Core..:? "PropertyNameSuggestions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSearchSuggestionsResponse' smart constructor.
data GetSearchSuggestionsResponse = GetSearchSuggestionsResponse'
  { -- | A list of property names for a @Resource@ that match a @SuggestionQuery@ .
    propertyNameSuggestions :: Core.Maybe [Types.PropertyNameSuggestion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSearchSuggestionsResponse' value with any optional fields omitted.
mkGetSearchSuggestionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSearchSuggestionsResponse
mkGetSearchSuggestionsResponse responseStatus =
  GetSearchSuggestionsResponse'
    { propertyNameSuggestions =
        Core.Nothing,
      responseStatus
    }

-- | A list of property names for a @Resource@ that match a @SuggestionQuery@ .
--
-- /Note:/ Consider using 'propertyNameSuggestions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsPropertyNameSuggestions :: Lens.Lens' GetSearchSuggestionsResponse (Core.Maybe [Types.PropertyNameSuggestion])
gssrrsPropertyNameSuggestions = Lens.field @"propertyNameSuggestions"
{-# DEPRECATED gssrrsPropertyNameSuggestions "Use generic-lens or generic-optics with 'propertyNameSuggestions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsResponseStatus :: Lens.Lens' GetSearchSuggestionsResponse Core.Int
gssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

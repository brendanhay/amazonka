{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of an intent.
--
-- The @GetIntentVersions@ operation returns an @IntentMetadata@ object for each version of an intent. For example, if an intent has three numbered versions, the @GetIntentVersions@ operation returns four @IntentMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
-- The @GetIntentVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetIntentVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntentVersions
  ( -- * Creating a request
    GetIntentVersions (..),
    mkGetIntentVersions,

    -- ** Request lenses
    givName,
    givMaxResults,
    givNextToken,

    -- * Destructuring the response
    GetIntentVersionsResponse (..),
    mkGetIntentVersionsResponse,

    -- ** Response lenses
    givrrsIntents,
    givrrsNextToken,
    givrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntentVersions' smart constructor.
data GetIntentVersions = GetIntentVersions'
  { -- | The name of the intent for which versions should be returned.
    name :: Types.IntentName,
    -- | The maximum number of intent versions to return in the response. The default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntentVersions' value with any optional fields omitted.
mkGetIntentVersions ::
  -- | 'name'
  Types.IntentName ->
  GetIntentVersions
mkGetIntentVersions name =
  GetIntentVersions'
    { name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the intent for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givName :: Lens.Lens' GetIntentVersions Types.IntentName
givName = Lens.field @"name"
{-# DEPRECATED givName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of intent versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givMaxResults :: Lens.Lens' GetIntentVersions (Core.Maybe Core.Natural)
givMaxResults = Lens.field @"maxResults"
{-# DEPRECATED givMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givNextToken :: Lens.Lens' GetIntentVersions (Core.Maybe Types.NextToken)
givNextToken = Lens.field @"nextToken"
{-# DEPRECATED givNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetIntentVersions where
  type Rs GetIntentVersions = GetIntentVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/intents/" Core.<> (Core.toText name) Core.<> ("/versions/")),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentVersionsResponse'
            Core.<$> (x Core..:? "intents")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetIntentVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"intents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetIntentVersionsResponse' smart constructor.
data GetIntentVersionsResponse = GetIntentVersionsResponse'
  { -- | An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
    intents :: Core.Maybe [Types.IntentMetadata],
    -- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetIntentVersionsResponse' value with any optional fields omitted.
mkGetIntentVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIntentVersionsResponse
mkGetIntentVersionsResponse responseStatus =
  GetIntentVersionsResponse'
    { intents = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsIntents :: Lens.Lens' GetIntentVersionsResponse (Core.Maybe [Types.IntentMetadata])
givrrsIntents = Lens.field @"intents"
{-# DEPRECATED givrrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsNextToken :: Lens.Lens' GetIntentVersionsResponse (Core.Maybe Types.NextToken)
givrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED givrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsResponseStatus :: Lens.Lens' GetIntentVersionsResponse Core.Int
givrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED givrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

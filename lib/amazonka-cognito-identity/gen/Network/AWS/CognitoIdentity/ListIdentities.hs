{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.ListIdentities
  ( -- * Creating a request
    ListIdentities (..),
    mkListIdentities,

    -- ** Request lenses
    liIdentityPoolId,
    liMaxResults,
    liHideDisabled,
    liNextToken,

    -- * Destructuring the response
    ListIdentitiesResponse (..),
    mkListIdentitiesResponse,

    -- ** Response lenses
    lirrsIdentities,
    lirrsIdentityPoolId,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the ListIdentities action.
--
-- /See:/ 'mkListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId,
    -- | The maximum number of identities to return.
    maxResults :: Core.Natural,
    -- | An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
    hideDisabled :: Core.Maybe Core.Bool,
    -- | A pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentities' value with any optional fields omitted.
mkListIdentities ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'maxResults'
  Core.Natural ->
  ListIdentities
mkListIdentities identityPoolId maxResults =
  ListIdentities'
    { identityPoolId,
      maxResults,
      hideDisabled = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIdentityPoolId :: Lens.Lens' ListIdentities Types.IdentityPoolId
liIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED liIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListIdentities Core.Natural
liMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
--
-- /Note:/ Consider using 'hideDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liHideDisabled :: Lens.Lens' ListIdentities (Core.Maybe Core.Bool)
liHideDisabled = Lens.field @"hideDisabled"
{-# DEPRECATED liHideDisabled "Use generic-lens or generic-optics with 'hideDisabled' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIdentities (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListIdentities where
  toJSON ListIdentities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("MaxResults" Core..= maxResults),
            ("HideDisabled" Core..=) Core.<$> hideDisabled,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListIdentities where
  type Rs ListIdentities = ListIdentitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.ListIdentities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentitiesResponse'
            Core.<$> (x Core..:? "Identities")
            Core.<*> (x Core..:? "IdentityPoolId")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to a ListIdentities request.
--
-- /See:/ 'mkListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { -- | An object containing a set of identities and associated mappings.
    identities :: Core.Maybe [Types.IdentityDescription],
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | A pagination token.
    nextToken :: Core.Maybe Types.PaginationKey,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListIdentitiesResponse' value with any optional fields omitted.
mkListIdentitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListIdentitiesResponse
mkListIdentitiesResponse responseStatus =
  ListIdentitiesResponse'
    { identities = Core.Nothing,
      identityPoolId = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An object containing a set of identities and associated mappings.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsIdentities :: Lens.Lens' ListIdentitiesResponse (Core.Maybe [Types.IdentityDescription])
lirrsIdentities = Lens.field @"identities"
{-# DEPRECATED lirrsIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsIdentityPoolId :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Types.IdentityPoolId)
lirrsIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED lirrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Types.PaginationKey)
lirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListIdentitiesResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

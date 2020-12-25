{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.ListDatasets
  ( -- * Creating a request
    ListDatasets (..),
    mkListDatasets,

    -- ** Request lenses
    ldIdentityId,
    ldIdentityPoolId,
    ldMaxResults,
    ldNextToken,

    -- * Destructuring the response
    ListDatasetsResponse (..),
    mkListDatasetsResponse,

    -- ** Response lenses
    ldrrsCount,
    ldrrsDatasets,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'mkListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Types.IdentityId,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId,
    -- | The maximum number of results to be returned.
    maxResults :: Core.Maybe Core.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDatasets' value with any optional fields omitted.
mkListDatasets ::
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  ListDatasets
mkListDatasets identityId identityPoolId =
  ListDatasets'
    { identityId,
      identityPoolId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityId :: Lens.Lens' ListDatasets Types.IdentityId
ldIdentityId = Lens.field @"identityId"
{-# DEPRECATED ldIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityPoolId :: Lens.Lens' ListDatasets Types.IdentityPoolId
ldIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED ldIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDatasets (Core.Maybe Core.Int)
ldMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDatasets (Core.Maybe Types.String)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDatasets where
  type Rs ListDatasets = ListDatasetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
                Core.<> ("/datasets")
            ),
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
          ListDatasetsResponse'
            Core.<$> (x Core..:? "Count")
            Core.<*> (x Core..:? "Datasets")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'mkListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | Number of datasets returned.
    count :: Core.Maybe Core.Int,
    -- | A set of datasets.
    datasets :: Core.Maybe [Types.Dataset],
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDatasetsResponse' value with any optional fields omitted.
mkListDatasetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDatasetsResponse
mkListDatasetsResponse responseStatus =
  ListDatasetsResponse'
    { count = Core.Nothing,
      datasets = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Number of datasets returned.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsCount :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Int)
ldrrsCount = Lens.field @"count"
{-# DEPRECATED ldrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | A set of datasets.
--
-- /Note:/ Consider using 'datasets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDatasets :: Lens.Lens' ListDatasetsResponse (Core.Maybe [Types.Dataset])
ldrrsDatasets = Lens.field @"datasets"
{-# DEPRECATED ldrrsDatasets "Use generic-lens or generic-optics with 'datasets' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDatasetsResponse (Core.Maybe Types.String)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDatasetsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API takes either a @ProvisonedProductId@ or a @ProvisionedProductName@ , along with a list of one or more output keys, and responds with the key/value pairs of those outputs.
module Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
  ( -- * Creating a request
    GetProvisionedProductOutputs (..),
    mkGetProvisionedProductOutputs,

    -- ** Request lenses
    gppoAcceptLanguage,
    gppoOutputKeys,
    gppoPageSize,
    gppoPageToken,
    gppoProvisionedProductId,
    gppoProvisionedProductName,

    -- * Destructuring the response
    GetProvisionedProductOutputsResponse (..),
    mkGetProvisionedProductOutputsResponse,

    -- ** Response lenses
    gpporrsNextPageToken,
    gpporrsOutputs,
    gpporrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkGetProvisionedProductOutputs' smart constructor.
data GetProvisionedProductOutputs = GetProvisionedProductOutputs'
  { -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
    outputKeys :: Core.Maybe [Types.OutputKey],
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken,
    -- | The identifier of the provisioned product that you want the outputs from.
    provisionedProductId :: Core.Maybe Types.ProvisionedProductId,
    -- | The name of the provisioned product that you want the outputs from.
    provisionedProductName :: Core.Maybe Types.ProvisionedProductName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedProductOutputs' value with any optional fields omitted.
mkGetProvisionedProductOutputs ::
  GetProvisionedProductOutputs
mkGetProvisionedProductOutputs =
  GetProvisionedProductOutputs'
    { acceptLanguage = Core.Nothing,
      outputKeys = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      provisionedProductId = Core.Nothing,
      provisionedProductName = Core.Nothing
    }

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoAcceptLanguage :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.AcceptLanguage)
gppoAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED gppoAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
--
-- /Note:/ Consider using 'outputKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoOutputKeys :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe [Types.OutputKey])
gppoOutputKeys = Lens.field @"outputKeys"
{-# DEPRECATED gppoOutputKeys "Use generic-lens or generic-optics with 'outputKeys' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageSize :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Core.Natural)
gppoPageSize = Lens.field @"pageSize"
{-# DEPRECATED gppoPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageToken :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.PageToken)
gppoPageToken = Lens.field @"pageToken"
{-# DEPRECATED gppoPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The identifier of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductId :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.ProvisionedProductId)
gppoProvisionedProductId = Lens.field @"provisionedProductId"
{-# DEPRECATED gppoProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The name of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductName :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.ProvisionedProductName)
gppoProvisionedProductName = Lens.field @"provisionedProductName"
{-# DEPRECATED gppoProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

instance Core.FromJSON GetProvisionedProductOutputs where
  toJSON GetProvisionedProductOutputs {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("OutputKeys" Core..=) Core.<$> outputKeys,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("ProvisionedProductId" Core..=) Core.<$> provisionedProductId,
            ("ProvisionedProductName" Core..=)
              Core.<$> provisionedProductName
          ]
      )

instance Core.AWSRequest GetProvisionedProductOutputs where
  type
    Rs GetProvisionedProductOutputs =
      GetProvisionedProductOutputsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.GetProvisionedProductOutputs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedProductOutputsResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "Outputs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetProvisionedProductOutputsResponse' smart constructor.
data GetProvisionedProductOutputsResponse = GetProvisionedProductOutputsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
    outputs :: Core.Maybe [Types.RecordOutput],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedProductOutputsResponse' value with any optional fields omitted.
mkGetProvisionedProductOutputsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProvisionedProductOutputsResponse
mkGetProvisionedProductOutputsResponse responseStatus =
  GetProvisionedProductOutputsResponse'
    { nextPageToken =
        Core.Nothing,
      outputs = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsNextPageToken :: Lens.Lens' GetProvisionedProductOutputsResponse (Core.Maybe Types.NextPageToken)
gpporrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gpporrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsOutputs :: Lens.Lens' GetProvisionedProductOutputsResponse (Core.Maybe [Types.RecordOutput])
gpporrsOutputs = Lens.field @"outputs"
{-# DEPRECATED gpporrsOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsResponseStatus :: Lens.Lens' GetProvisionedProductOutputsResponse Core.Int
gpporrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpporrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

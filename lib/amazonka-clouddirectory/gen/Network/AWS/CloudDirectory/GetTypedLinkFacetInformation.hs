{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the identity attribute order for a specific 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
  ( -- * Creating a request
    GetTypedLinkFacetInformation (..),
    mkGetTypedLinkFacetInformation,

    -- ** Request lenses
    gtlfiSchemaArn,
    gtlfiName,

    -- * Destructuring the response
    GetTypedLinkFacetInformationResponse (..),
    mkGetTypedLinkFacetInformationResponse,

    -- ** Response lenses
    gtlfirrsIdentityAttributeOrder,
    gtlfirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTypedLinkFacetInformation' smart constructor.
data GetTypedLinkFacetInformation = GetTypedLinkFacetInformation'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaArn :: Types.SchemaArn,
    -- | The unique name of the typed link facet.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTypedLinkFacetInformation' value with any optional fields omitted.
mkGetTypedLinkFacetInformation ::
  -- | 'schemaArn'
  Types.SchemaArn ->
  -- | 'name'
  Types.Name ->
  GetTypedLinkFacetInformation
mkGetTypedLinkFacetInformation schemaArn name =
  GetTypedLinkFacetInformation' {schemaArn, name}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfiSchemaArn :: Lens.Lens' GetTypedLinkFacetInformation Types.SchemaArn
gtlfiSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED gtlfiSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfiName :: Lens.Lens' GetTypedLinkFacetInformation Types.Name
gtlfiName = Lens.field @"name"
{-# DEPRECATED gtlfiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetTypedLinkFacetInformation where
  toJSON GetTypedLinkFacetInformation {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetTypedLinkFacetInformation where
  type
    Rs GetTypedLinkFacetInformation =
      GetTypedLinkFacetInformationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/typedlink/facet/get",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTypedLinkFacetInformationResponse'
            Core.<$> (x Core..:? "IdentityAttributeOrder")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTypedLinkFacetInformationResponse' smart constructor.
data GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse'
  { -- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    identityAttributeOrder :: Core.Maybe [Types.AttributeName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTypedLinkFacetInformationResponse' value with any optional fields omitted.
mkGetTypedLinkFacetInformationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTypedLinkFacetInformationResponse
mkGetTypedLinkFacetInformationResponse responseStatus =
  GetTypedLinkFacetInformationResponse'
    { identityAttributeOrder =
        Core.Nothing,
      responseStatus
    }

-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfirrsIdentityAttributeOrder :: Lens.Lens' GetTypedLinkFacetInformationResponse (Core.Maybe [Types.AttributeName])
gtlfirrsIdentityAttributeOrder = Lens.field @"identityAttributeOrder"
{-# DEPRECATED gtlfirrsIdentityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfirrsResponseStatus :: Lens.Lens' GetTypedLinkFacetInformationResponse Core.Int
gtlfirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtlfirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

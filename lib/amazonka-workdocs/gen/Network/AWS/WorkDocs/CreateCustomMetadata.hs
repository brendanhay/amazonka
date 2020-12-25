{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateCustomMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more custom properties to the specified resource (a folder, document, or version).
module Network.AWS.WorkDocs.CreateCustomMetadata
  ( -- * Creating a request
    CreateCustomMetadata (..),
    mkCreateCustomMetadata,

    -- ** Request lenses
    ccmResourceId,
    ccmCustomMetadata,
    ccmAuthenticationToken,
    ccmVersionId,

    -- * Destructuring the response
    CreateCustomMetadataResponse (..),
    mkCreateCustomMetadataResponse,

    -- ** Response lenses
    ccmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateCustomMetadata' smart constructor.
data CreateCustomMetadata = CreateCustomMetadata'
  { -- | The ID of the resource.
    resourceId :: Types.ResourceIdType,
    -- | Custom metadata in the form of name-value pairs.
    customMetadata :: Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The ID of the version, if the custom metadata is being added to a document version.
    versionId :: Core.Maybe Types.DocumentVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomMetadata' value with any optional fields omitted.
mkCreateCustomMetadata ::
  -- | 'resourceId'
  Types.ResourceIdType ->
  CreateCustomMetadata
mkCreateCustomMetadata resourceId =
  CreateCustomMetadata'
    { resourceId,
      customMetadata = Core.mempty,
      authenticationToken = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmResourceId :: Lens.Lens' CreateCustomMetadata Types.ResourceIdType
ccmResourceId = Lens.field @"resourceId"
{-# DEPRECATED ccmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Custom metadata in the form of name-value pairs.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmCustomMetadata :: Lens.Lens' CreateCustomMetadata (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType)
ccmCustomMetadata = Lens.field @"customMetadata"
{-# DEPRECATED ccmCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmAuthenticationToken :: Lens.Lens' CreateCustomMetadata (Core.Maybe Types.AuthenticationHeaderType)
ccmAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED ccmAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the version, if the custom metadata is being added to a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmVersionId :: Lens.Lens' CreateCustomMetadata (Core.Maybe Types.DocumentVersionIdType)
ccmVersionId = Lens.field @"versionId"
{-# DEPRECATED ccmVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromJSON CreateCustomMetadata where
  toJSON CreateCustomMetadata {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CustomMetadata" Core..= customMetadata)]
      )

instance Core.AWSRequest CreateCustomMetadata where
  type Rs CreateCustomMetadata = CreateCustomMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/customMetadata")
            ),
        Core._rqQuery = Core.toQueryValue "versionid" Core.<$> versionId,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCustomMetadataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCustomMetadataResponse' smart constructor.
newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomMetadataResponse' value with any optional fields omitted.
mkCreateCustomMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCustomMetadataResponse
mkCreateCustomMetadataResponse responseStatus =
  CreateCustomMetadataResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsResponseStatus :: Lens.Lens' CreateCustomMetadataResponse Core.Int
ccmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCustomMetadata (..)
    , mkCreateCustomMetadata
    -- ** Request lenses
    , ccmResourceId
    , ccmCustomMetadata
    , ccmAuthenticationToken
    , ccmVersionId

    -- * Destructuring the response
    , CreateCustomMetadataResponse (..)
    , mkCreateCustomMetadataResponse
    -- ** Response lenses
    , ccmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateCustomMetadata' smart constructor.
data CreateCustomMetadata = CreateCustomMetadata'
  { resourceId :: Types.ResourceIdType
    -- ^ The ID of the resource.
  , customMetadata :: Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType
    -- ^ Custom metadata in the form of name-value pairs.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , versionId :: Core.Maybe Types.DocumentVersionIdType
    -- ^ The ID of the version, if the custom metadata is being added to a document version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomMetadata' value with any optional fields omitted.
mkCreateCustomMetadata
    :: Types.ResourceIdType -- ^ 'resourceId'
    -> CreateCustomMetadata
mkCreateCustomMetadata resourceId
  = CreateCustomMetadata'{resourceId, customMetadata = Core.mempty,
                          authenticationToken = Core.Nothing, versionId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmResourceId :: Lens.Lens' CreateCustomMetadata Types.ResourceIdType
ccmResourceId = Lens.field @"resourceId"
{-# INLINEABLE ccmResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Custom metadata in the form of name-value pairs.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmCustomMetadata :: Lens.Lens' CreateCustomMetadata (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType)
ccmCustomMetadata = Lens.field @"customMetadata"
{-# INLINEABLE ccmCustomMetadata #-}
{-# DEPRECATED customMetadata "Use generic-lens or generic-optics with 'customMetadata' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmAuthenticationToken :: Lens.Lens' CreateCustomMetadata (Core.Maybe Types.AuthenticationHeaderType)
ccmAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE ccmAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The ID of the version, if the custom metadata is being added to a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmVersionId :: Lens.Lens' CreateCustomMetadata (Core.Maybe Types.DocumentVersionIdType)
ccmVersionId = Lens.field @"versionId"
{-# INLINEABLE ccmVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery CreateCustomMetadata where
        toQuery CreateCustomMetadata{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionid") versionId

instance Core.ToHeaders CreateCustomMetadata where
        toHeaders CreateCustomMetadata{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCustomMetadata where
        toJSON CreateCustomMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CustomMetadata" Core..= customMetadata)])

instance Core.AWSRequest CreateCustomMetadata where
        type Rs CreateCustomMetadata = CreateCustomMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/api/v1/resources/" Core.<> Core.toText resourceId Core.<>
                             "/customMetadata",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateCustomMetadataResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCustomMetadataResponse' smart constructor.
newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomMetadataResponse' value with any optional fields omitted.
mkCreateCustomMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCustomMetadataResponse
mkCreateCustomMetadataResponse responseStatus
  = CreateCustomMetadataResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsResponseStatus :: Lens.Lens' CreateCustomMetadataResponse Core.Int
ccmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

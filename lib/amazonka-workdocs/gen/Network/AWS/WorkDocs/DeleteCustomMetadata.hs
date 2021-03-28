{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
module Network.AWS.WorkDocs.DeleteCustomMetadata
    (
    -- * Creating a request
      DeleteCustomMetadata (..)
    , mkDeleteCustomMetadata
    -- ** Request lenses
    , dcmResourceId
    , dcmAuthenticationToken
    , dcmDeleteAll
    , dcmKeys
    , dcmVersionId

    -- * Destructuring the response
    , DeleteCustomMetadataResponse (..)
    , mkDeleteCustomMetadataResponse
    -- ** Response lenses
    , dcmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { resourceId :: Types.ResourceIdType
    -- ^ The ID of the resource, either a document or folder.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , deleteAll :: Core.Maybe Core.Bool
    -- ^ Flag to indicate removal of all custom metadata properties from the specified resource.
  , keys :: Core.Maybe [Types.CustomMetadataKeyType]
    -- ^ List of properties to remove.
  , versionId :: Core.Maybe Types.DocumentVersionIdType
    -- ^ The ID of the version, if the custom metadata is being deleted from a document version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomMetadata' value with any optional fields omitted.
mkDeleteCustomMetadata
    :: Types.ResourceIdType -- ^ 'resourceId'
    -> DeleteCustomMetadata
mkDeleteCustomMetadata resourceId
  = DeleteCustomMetadata'{resourceId,
                          authenticationToken = Core.Nothing, deleteAll = Core.Nothing,
                          keys = Core.Nothing, versionId = Core.Nothing}

-- | The ID of the resource, either a document or folder.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmResourceId :: Lens.Lens' DeleteCustomMetadata Types.ResourceIdType
dcmResourceId = Lens.field @"resourceId"
{-# INLINEABLE dcmResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmAuthenticationToken :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Types.AuthenticationHeaderType)
dcmAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dcmAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | Flag to indicate removal of all custom metadata properties from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmDeleteAll :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Core.Bool)
dcmDeleteAll = Lens.field @"deleteAll"
{-# INLINEABLE dcmDeleteAll #-}
{-# DEPRECATED deleteAll "Use generic-lens or generic-optics with 'deleteAll' instead"  #-}

-- | List of properties to remove.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmKeys :: Lens.Lens' DeleteCustomMetadata (Core.Maybe [Types.CustomMetadataKeyType])
dcmKeys = Lens.field @"keys"
{-# INLINEABLE dcmKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

-- | The ID of the version, if the custom metadata is being deleted from a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmVersionId :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Types.DocumentVersionIdType)
dcmVersionId = Lens.field @"versionId"
{-# INLINEABLE dcmVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DeleteCustomMetadata where
        toQuery DeleteCustomMetadata{..}
          = Core.maybe Core.mempty (Core.toQueryPair "deleteAll") deleteAll
              Core.<>
              Core.toQueryPair "keys"
                (Core.maybe Core.mempty (Core.toQueryList "member") keys)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId

instance Core.ToHeaders DeleteCustomMetadata where
        toHeaders DeleteCustomMetadata{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteCustomMetadata where
        type Rs DeleteCustomMetadata = DeleteCustomMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/api/v1/resources/" Core.<> Core.toText resourceId Core.<>
                             "/customMetadata",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCustomMetadataResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCustomMetadataResponse' smart constructor.
newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomMetadataResponse' value with any optional fields omitted.
mkDeleteCustomMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCustomMetadataResponse
mkDeleteCustomMetadataResponse responseStatus
  = DeleteCustomMetadataResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrrsResponseStatus :: Lens.Lens' DeleteCustomMetadataResponse Core.Int
dcmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

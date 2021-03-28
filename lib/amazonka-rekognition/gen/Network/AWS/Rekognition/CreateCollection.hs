{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a collection in an AWS Region. You can add faces to the collection using the 'IndexFaces' operation. 
--
-- For example, you might create collections, one for each of your application users. A user can then index faces using the @IndexFaces@ operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container. 
-- When you create a collection, it is associated with the latest version of the face model version.
-- This operation requires permissions to perform the @rekognition:CreateCollection@ action.
module Network.AWS.Rekognition.CreateCollection
    (
    -- * Creating a request
      CreateCollection (..)
    , mkCreateCollection
    -- ** Request lenses
    , ccCollectionId

    -- * Destructuring the response
    , CreateCollectionResponse (..)
    , mkCreateCollectionResponse
    -- ** Response lenses
    , ccrrsCollectionArn
    , ccrrsFaceModelVersion
    , ccrrsStatusCode
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCollection' smart constructor.
newtype CreateCollection = CreateCollection'
  { collectionId :: Types.CollectionId
    -- ^ ID for the collection that you are creating.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCollection' value with any optional fields omitted.
mkCreateCollection
    :: Types.CollectionId -- ^ 'collectionId'
    -> CreateCollection
mkCreateCollection collectionId = CreateCollection'{collectionId}

-- | ID for the collection that you are creating.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionId :: Lens.Lens' CreateCollection Types.CollectionId
ccCollectionId = Lens.field @"collectionId"
{-# INLINEABLE ccCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

instance Core.ToQuery CreateCollection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCollection where
        toHeaders CreateCollection{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.CreateCollection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCollection where
        toJSON CreateCollection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CollectionId" Core..= collectionId)])

instance Core.AWSRequest CreateCollection where
        type Rs CreateCollection = CreateCollectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCollectionResponse' Core.<$>
                   (x Core..:? "CollectionArn") Core.<*> x Core..:? "FaceModelVersion"
                     Core.<*> x Core..:? "StatusCode"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { collectionArn :: Core.Maybe Core.Text
    -- ^ Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources. 
  , faceModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the face detection model associated with the collection you are creating.
  , statusCode :: Core.Maybe Core.Natural
    -- ^ HTTP status code indicating the result of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCollectionResponse' value with any optional fields omitted.
mkCreateCollectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCollectionResponse
mkCreateCollectionResponse responseStatus
  = CreateCollectionResponse'{collectionArn = Core.Nothing,
                              faceModelVersion = Core.Nothing, statusCode = Core.Nothing,
                              responseStatus}

-- | Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources. 
--
-- /Note:/ Consider using 'collectionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCollectionArn :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Text)
ccrrsCollectionArn = Lens.field @"collectionArn"
{-# INLINEABLE ccrrsCollectionArn #-}
{-# DEPRECATED collectionArn "Use generic-lens or generic-optics with 'collectionArn' instead"  #-}

-- | Version number of the face detection model associated with the collection you are creating.
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsFaceModelVersion :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Text)
ccrrsFaceModelVersion = Lens.field @"faceModelVersion"
{-# INLINEABLE ccrrsFaceModelVersion #-}
{-# DEPRECATED faceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead"  #-}

-- | HTTP status code indicating the result of the operation.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsStatusCode :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Natural)
ccrrsStatusCode = Lens.field @"statusCode"
{-# INLINEABLE ccrrsStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCollectionResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

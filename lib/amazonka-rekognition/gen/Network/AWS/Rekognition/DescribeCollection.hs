{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified collection. You can use @DescribeCollection@ to get information, such as the number of faces indexed into a collection and the version of the model used by the collection for face detection.
--
-- For more information, see Describing a Collection in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.DescribeCollection
    (
    -- * Creating a request
      DescribeCollection (..)
    , mkDescribeCollection
    -- ** Request lenses
    , dCollectionId

    -- * Destructuring the response
    , DescribeCollectionResponse (..)
    , mkDescribeCollectionResponse
    -- ** Response lenses
    , drsCollectionARN
    , drsCreationTimestamp
    , drsFaceCount
    , drsFaceModelVersion
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCollection' smart constructor.
newtype DescribeCollection = DescribeCollection'
  { collectionId :: Types.CollectionId
    -- ^ The ID of the collection to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCollection' value with any optional fields omitted.
mkDescribeCollection
    :: Types.CollectionId -- ^ 'collectionId'
    -> DescribeCollection
mkDescribeCollection collectionId
  = DescribeCollection'{collectionId}

-- | The ID of the collection to describe.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCollectionId :: Lens.Lens' DescribeCollection Types.CollectionId
dCollectionId = Lens.field @"collectionId"
{-# INLINEABLE dCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

instance Core.ToQuery DescribeCollection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCollection where
        toHeaders DescribeCollection{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DescribeCollection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCollection where
        toJSON DescribeCollection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CollectionId" Core..= collectionId)])

instance Core.AWSRequest DescribeCollection where
        type Rs DescribeCollection = DescribeCollectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCollectionResponse' Core.<$>
                   (x Core..:? "CollectionARN") Core.<*>
                     x Core..:? "CreationTimestamp"
                     Core.<*> x Core..:? "FaceCount"
                     Core.<*> x Core..:? "FaceModelVersion"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCollectionResponse' smart constructor.
data DescribeCollectionResponse = DescribeCollectionResponse'
  { collectionARN :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the collection.
  , creationTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
  , faceCount :: Core.Maybe Core.Natural
    -- ^ The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
  , faceModelVersion :: Core.Maybe Core.Text
    -- ^ The version of the face model that's used by the collection for face detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCollectionResponse' value with any optional fields omitted.
mkDescribeCollectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCollectionResponse
mkDescribeCollectionResponse responseStatus
  = DescribeCollectionResponse'{collectionARN = Core.Nothing,
                                creationTimestamp = Core.Nothing, faceCount = Core.Nothing,
                                faceModelVersion = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the collection.
--
-- /Note:/ Consider using 'collectionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCollectionARN :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Text)
drsCollectionARN = Lens.field @"collectionARN"
{-# INLINEABLE drsCollectionARN #-}
{-# DEPRECATED collectionARN "Use generic-lens or generic-optics with 'collectionARN' instead"  #-}

-- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationTimestamp :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.NominalDiffTime)
drsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE drsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
--
-- /Note:/ Consider using 'faceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceCount :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Natural)
drsFaceCount = Lens.field @"faceCount"
{-# INLINEABLE drsFaceCount #-}
{-# DEPRECATED faceCount "Use generic-lens or generic-optics with 'faceCount' instead"  #-}

-- | The version of the face model that's used by the collection for face detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceModelVersion :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Text)
drsFaceModelVersion = Lens.field @"faceModelVersion"
{-# INLINEABLE drsFaceModelVersion #-}
{-# DEPRECATED faceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCollectionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

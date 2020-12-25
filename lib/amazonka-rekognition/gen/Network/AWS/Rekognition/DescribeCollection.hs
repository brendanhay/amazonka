{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeCollection (..),
    mkDescribeCollection,

    -- ** Request lenses
    dCollectionId,

    -- * Destructuring the response
    DescribeCollectionResponse (..),
    mkDescribeCollectionResponse,

    -- ** Response lenses
    drsCollectionARN,
    drsCreationTimestamp,
    drsFaceCount,
    drsFaceModelVersion,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCollection' smart constructor.
newtype DescribeCollection = DescribeCollection'
  { -- | The ID of the collection to describe.
    collectionId :: Types.CollectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCollection' value with any optional fields omitted.
mkDescribeCollection ::
  -- | 'collectionId'
  Types.CollectionId ->
  DescribeCollection
mkDescribeCollection collectionId =
  DescribeCollection' {collectionId}

-- | The ID of the collection to describe.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCollectionId :: Lens.Lens' DescribeCollection Types.CollectionId
dCollectionId = Lens.field @"collectionId"
{-# DEPRECATED dCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Core.FromJSON DescribeCollection where
  toJSON DescribeCollection {..} =
    Core.object
      (Core.catMaybes [Core.Just ("CollectionId" Core..= collectionId)])

instance Core.AWSRequest DescribeCollection where
  type Rs DescribeCollection = DescribeCollectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.DescribeCollection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCollectionResponse'
            Core.<$> (x Core..:? "CollectionARN")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "FaceCount")
            Core.<*> (x Core..:? "FaceModelVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCollectionResponse' smart constructor.
data DescribeCollectionResponse = DescribeCollectionResponse'
  { -- | The Amazon Resource Name (ARN) of the collection.
    collectionARN :: Core.Maybe Types.String,
    -- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
    creationTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
    faceCount :: Core.Maybe Core.Natural,
    -- | The version of the face model that's used by the collection for face detection.
    --
    -- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
    faceModelVersion :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCollectionResponse' value with any optional fields omitted.
mkDescribeCollectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCollectionResponse
mkDescribeCollectionResponse responseStatus =
  DescribeCollectionResponse'
    { collectionARN = Core.Nothing,
      creationTimestamp = Core.Nothing,
      faceCount = Core.Nothing,
      faceModelVersion = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the collection.
--
-- /Note:/ Consider using 'collectionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCollectionARN :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Types.String)
drsCollectionARN = Lens.field @"collectionARN"
{-# DEPRECATED drsCollectionARN "Use generic-lens or generic-optics with 'collectionARN' instead." #-}

-- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationTimestamp :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.NominalDiffTime)
drsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED drsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
--
-- /Note:/ Consider using 'faceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceCount :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Natural)
drsFaceCount = Lens.field @"faceCount"
{-# DEPRECATED drsFaceCount "Use generic-lens or generic-optics with 'faceCount' instead." #-}

-- | The version of the face model that's used by the collection for face detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceModelVersion :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Types.String)
drsFaceModelVersion = Lens.field @"faceModelVersion"
{-# DEPRECATED drsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCollectionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

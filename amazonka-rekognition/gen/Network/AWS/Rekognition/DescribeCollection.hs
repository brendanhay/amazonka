{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified collection. You can use @DescribeCollection@ to
-- get information, such as the number of faces indexed into a collection
-- and the version of the model used by the collection for face detection.
--
-- For more information, see Describing a Collection in the Amazon
-- Rekognition Developer Guide.
module Network.AWS.Rekognition.DescribeCollection
  ( -- * Creating a Request
    DescribeCollection (..),
    newDescribeCollection,

    -- * Request Lenses
    describeCollection_collectionId,

    -- * Destructuring the Response
    DescribeCollectionResponse (..),
    newDescribeCollectionResponse,

    -- * Response Lenses
    describeCollectionResponse_creationTimestamp,
    describeCollectionResponse_faceModelVersion,
    describeCollectionResponse_collectionARN,
    describeCollectionResponse_faceCount,
    describeCollectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCollection' smart constructor.
data DescribeCollection = DescribeCollection'
  { -- | The ID of the collection to describe.
    collectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'describeCollection_collectionId' - The ID of the collection to describe.
newDescribeCollection ::
  -- | 'collectionId'
  Core.Text ->
  DescribeCollection
newDescribeCollection pCollectionId_ =
  DescribeCollection' {collectionId = pCollectionId_}

-- | The ID of the collection to describe.
describeCollection_collectionId :: Lens.Lens' DescribeCollection Core.Text
describeCollection_collectionId = Lens.lens (\DescribeCollection' {collectionId} -> collectionId) (\s@DescribeCollection' {} a -> s {collectionId = a} :: DescribeCollection)

instance Core.AWSRequest DescribeCollection where
  type
    AWSResponse DescribeCollection =
      DescribeCollectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCollectionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "FaceModelVersion")
            Core.<*> (x Core..?> "CollectionARN")
            Core.<*> (x Core..?> "FaceCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCollection

instance Core.NFData DescribeCollection

instance Core.ToHeaders DescribeCollection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DescribeCollection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCollection where
  toJSON DescribeCollection' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CollectionId" Core..= collectionId)]
      )

instance Core.ToPath DescribeCollection where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCollection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCollectionResponse' smart constructor.
data DescribeCollectionResponse = DescribeCollectionResponse'
  { -- | The number of milliseconds since the Unix epoch time until the creation
    -- of the collection. The Unix epoch time is 00:00:00 Coordinated Universal
    -- Time (UTC), Thursday, 1 January 1970.
    creationTimestamp :: Core.Maybe Core.POSIX,
    -- | The version of the face model that\'s used by the collection for face
    -- detection.
    --
    -- For more information, see Model Versioning in the Amazon Rekognition
    -- Developer Guide.
    faceModelVersion :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the collection.
    collectionARN :: Core.Maybe Core.Text,
    -- | The number of faces that are indexed into the collection. To index faces
    -- into a collection, use IndexFaces.
    faceCount :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'describeCollectionResponse_creationTimestamp' - The number of milliseconds since the Unix epoch time until the creation
-- of the collection. The Unix epoch time is 00:00:00 Coordinated Universal
-- Time (UTC), Thursday, 1 January 1970.
--
-- 'faceModelVersion', 'describeCollectionResponse_faceModelVersion' - The version of the face model that\'s used by the collection for face
-- detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition
-- Developer Guide.
--
-- 'collectionARN', 'describeCollectionResponse_collectionARN' - The Amazon Resource Name (ARN) of the collection.
--
-- 'faceCount', 'describeCollectionResponse_faceCount' - The number of faces that are indexed into the collection. To index faces
-- into a collection, use IndexFaces.
--
-- 'httpStatus', 'describeCollectionResponse_httpStatus' - The response's http status code.
newDescribeCollectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCollectionResponse
newDescribeCollectionResponse pHttpStatus_ =
  DescribeCollectionResponse'
    { creationTimestamp =
        Core.Nothing,
      faceModelVersion = Core.Nothing,
      collectionARN = Core.Nothing,
      faceCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of milliseconds since the Unix epoch time until the creation
-- of the collection. The Unix epoch time is 00:00:00 Coordinated Universal
-- Time (UTC), Thursday, 1 January 1970.
describeCollectionResponse_creationTimestamp :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.UTCTime)
describeCollectionResponse_creationTimestamp = Lens.lens (\DescribeCollectionResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeCollectionResponse' {} a -> s {creationTimestamp = a} :: DescribeCollectionResponse) Core.. Lens.mapping Core._Time

-- | The version of the face model that\'s used by the collection for face
-- detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition
-- Developer Guide.
describeCollectionResponse_faceModelVersion :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Text)
describeCollectionResponse_faceModelVersion = Lens.lens (\DescribeCollectionResponse' {faceModelVersion} -> faceModelVersion) (\s@DescribeCollectionResponse' {} a -> s {faceModelVersion = a} :: DescribeCollectionResponse)

-- | The Amazon Resource Name (ARN) of the collection.
describeCollectionResponse_collectionARN :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Text)
describeCollectionResponse_collectionARN = Lens.lens (\DescribeCollectionResponse' {collectionARN} -> collectionARN) (\s@DescribeCollectionResponse' {} a -> s {collectionARN = a} :: DescribeCollectionResponse)

-- | The number of faces that are indexed into the collection. To index faces
-- into a collection, use IndexFaces.
describeCollectionResponse_faceCount :: Lens.Lens' DescribeCollectionResponse (Core.Maybe Core.Natural)
describeCollectionResponse_faceCount = Lens.lens (\DescribeCollectionResponse' {faceCount} -> faceCount) (\s@DescribeCollectionResponse' {} a -> s {faceCount = a} :: DescribeCollectionResponse)

-- | The response's http status code.
describeCollectionResponse_httpStatus :: Lens.Lens' DescribeCollectionResponse Core.Int
describeCollectionResponse_httpStatus = Lens.lens (\DescribeCollectionResponse' {httpStatus} -> httpStatus) (\s@DescribeCollectionResponse' {} a -> s {httpStatus = a} :: DescribeCollectionResponse)

instance Core.NFData DescribeCollectionResponse

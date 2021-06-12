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
-- Module      : Network.AWS.Rekognition.CreateCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a collection in an AWS Region. You can add faces to the
-- collection using the IndexFaces operation.
--
-- For example, you might create collections, one for each of your
-- application users. A user can then index faces using the @IndexFaces@
-- operation and persist results in a specific collection. Then, a user can
-- search the collection for faces in the user-specific container.
--
-- When you create a collection, it is associated with the latest version
-- of the face model version.
--
-- Collection names are case-sensitive.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateCollection@ action.
module Network.AWS.Rekognition.CreateCollection
  ( -- * Creating a Request
    CreateCollection (..),
    newCreateCollection,

    -- * Request Lenses
    createCollection_collectionId,

    -- * Destructuring the Response
    CreateCollectionResponse (..),
    newCreateCollectionResponse,

    -- * Response Lenses
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_collectionArn,
    createCollectionResponse_statusCode,
    createCollectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCollection' smart constructor.
data CreateCollection = CreateCollection'
  { -- | ID for the collection that you are creating.
    collectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'createCollection_collectionId' - ID for the collection that you are creating.
newCreateCollection ::
  -- | 'collectionId'
  Core.Text ->
  CreateCollection
newCreateCollection pCollectionId_ =
  CreateCollection' {collectionId = pCollectionId_}

-- | ID for the collection that you are creating.
createCollection_collectionId :: Lens.Lens' CreateCollection Core.Text
createCollection_collectionId = Lens.lens (\CreateCollection' {collectionId} -> collectionId) (\s@CreateCollection' {} a -> s {collectionId = a} :: CreateCollection)

instance Core.AWSRequest CreateCollection where
  type
    AWSResponse CreateCollection =
      CreateCollectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCollectionResponse'
            Core.<$> (x Core..?> "FaceModelVersion")
            Core.<*> (x Core..?> "CollectionArn")
            Core.<*> (x Core..?> "StatusCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCollection

instance Core.NFData CreateCollection

instance Core.ToHeaders CreateCollection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.CreateCollection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCollection where
  toJSON CreateCollection' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CollectionId" Core..= collectionId)]
      )

instance Core.ToPath CreateCollection where
  toPath = Core.const "/"

instance Core.ToQuery CreateCollection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { -- | Version number of the face detection model associated with the
    -- collection you are creating.
    faceModelVersion :: Core.Maybe Core.Text,
    -- | Amazon Resource Name (ARN) of the collection. You can use this to manage
    -- permissions on your resources.
    collectionArn :: Core.Maybe Core.Text,
    -- | HTTP status code indicating the result of the operation.
    statusCode :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'createCollectionResponse_faceModelVersion' - Version number of the face detection model associated with the
-- collection you are creating.
--
-- 'collectionArn', 'createCollectionResponse_collectionArn' - Amazon Resource Name (ARN) of the collection. You can use this to manage
-- permissions on your resources.
--
-- 'statusCode', 'createCollectionResponse_statusCode' - HTTP status code indicating the result of the operation.
--
-- 'httpStatus', 'createCollectionResponse_httpStatus' - The response's http status code.
newCreateCollectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCollectionResponse
newCreateCollectionResponse pHttpStatus_ =
  CreateCollectionResponse'
    { faceModelVersion =
        Core.Nothing,
      collectionArn = Core.Nothing,
      statusCode = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the
-- collection you are creating.
createCollectionResponse_faceModelVersion :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Text)
createCollectionResponse_faceModelVersion = Lens.lens (\CreateCollectionResponse' {faceModelVersion} -> faceModelVersion) (\s@CreateCollectionResponse' {} a -> s {faceModelVersion = a} :: CreateCollectionResponse)

-- | Amazon Resource Name (ARN) of the collection. You can use this to manage
-- permissions on your resources.
createCollectionResponse_collectionArn :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Text)
createCollectionResponse_collectionArn = Lens.lens (\CreateCollectionResponse' {collectionArn} -> collectionArn) (\s@CreateCollectionResponse' {} a -> s {collectionArn = a} :: CreateCollectionResponse)

-- | HTTP status code indicating the result of the operation.
createCollectionResponse_statusCode :: Lens.Lens' CreateCollectionResponse (Core.Maybe Core.Natural)
createCollectionResponse_statusCode = Lens.lens (\CreateCollectionResponse' {statusCode} -> statusCode) (\s@CreateCollectionResponse' {} a -> s {statusCode = a} :: CreateCollectionResponse)

-- | The response's http status code.
createCollectionResponse_httpStatus :: Lens.Lens' CreateCollectionResponse Core.Int
createCollectionResponse_httpStatus = Lens.lens (\CreateCollectionResponse' {httpStatus} -> httpStatus) (\s@CreateCollectionResponse' {} a -> s {httpStatus = a} :: CreateCollectionResponse)

instance Core.NFData CreateCollectionResponse

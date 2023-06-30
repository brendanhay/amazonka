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
-- Module      : Amazonka.Rekognition.CreateCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- @rekognition:CreateCollection@ action. If you want to tag your
-- collection, you also require permission to perform the
-- @rekognition:TagResource@ operation.
module Amazonka.Rekognition.CreateCollection
  ( -- * Creating a Request
    CreateCollection (..),
    newCreateCollection,

    -- * Request Lenses
    createCollection_tags,
    createCollection_collectionId,

    -- * Destructuring the Response
    CreateCollectionResponse (..),
    newCreateCollectionResponse,

    -- * Response Lenses
    createCollectionResponse_collectionArn,
    createCollectionResponse_faceModelVersion,
    createCollectionResponse_statusCode,
    createCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCollection' smart constructor.
data CreateCollection = CreateCollection'
  { -- | A set of tags (key-value pairs) that you want to attach to the
    -- collection.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ID for the collection that you are creating.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCollection_tags' - A set of tags (key-value pairs) that you want to attach to the
-- collection.
--
-- 'collectionId', 'createCollection_collectionId' - ID for the collection that you are creating.
newCreateCollection ::
  -- | 'collectionId'
  Prelude.Text ->
  CreateCollection
newCreateCollection pCollectionId_ =
  CreateCollection'
    { tags = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | A set of tags (key-value pairs) that you want to attach to the
-- collection.
createCollection_tags :: Lens.Lens' CreateCollection (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCollection_tags = Lens.lens (\CreateCollection' {tags} -> tags) (\s@CreateCollection' {} a -> s {tags = a} :: CreateCollection) Prelude.. Lens.mapping Lens.coerced

-- | ID for the collection that you are creating.
createCollection_collectionId :: Lens.Lens' CreateCollection Prelude.Text
createCollection_collectionId = Lens.lens (\CreateCollection' {collectionId} -> collectionId) (\s@CreateCollection' {} a -> s {collectionId = a} :: CreateCollection)

instance Core.AWSRequest CreateCollection where
  type
    AWSResponse CreateCollection =
      CreateCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCollectionResponse'
            Prelude.<$> (x Data..?> "CollectionArn")
            Prelude.<*> (x Data..?> "FaceModelVersion")
            Prelude.<*> (x Data..?> "StatusCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCollection where
  hashWithSalt _salt CreateCollection' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData CreateCollection where
  rnf CreateCollection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf collectionId

instance Data.ToHeaders CreateCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CreateCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCollection where
  toJSON CreateCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("CollectionId" Data..= collectionId)
          ]
      )

instance Data.ToPath CreateCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { -- | Amazon Resource Name (ARN) of the collection. You can use this to manage
    -- permissions on your resources.
    collectionArn :: Prelude.Maybe Prelude.Text,
    -- | Version number of the face detection model associated with the
    -- collection you are creating.
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | HTTP status code indicating the result of the operation.
    statusCode :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionArn', 'createCollectionResponse_collectionArn' - Amazon Resource Name (ARN) of the collection. You can use this to manage
-- permissions on your resources.
--
-- 'faceModelVersion', 'createCollectionResponse_faceModelVersion' - Version number of the face detection model associated with the
-- collection you are creating.
--
-- 'statusCode', 'createCollectionResponse_statusCode' - HTTP status code indicating the result of the operation.
--
-- 'httpStatus', 'createCollectionResponse_httpStatus' - The response's http status code.
newCreateCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCollectionResponse
newCreateCollectionResponse pHttpStatus_ =
  CreateCollectionResponse'
    { collectionArn =
        Prelude.Nothing,
      faceModelVersion = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon Resource Name (ARN) of the collection. You can use this to manage
-- permissions on your resources.
createCollectionResponse_collectionArn :: Lens.Lens' CreateCollectionResponse (Prelude.Maybe Prelude.Text)
createCollectionResponse_collectionArn = Lens.lens (\CreateCollectionResponse' {collectionArn} -> collectionArn) (\s@CreateCollectionResponse' {} a -> s {collectionArn = a} :: CreateCollectionResponse)

-- | Version number of the face detection model associated with the
-- collection you are creating.
createCollectionResponse_faceModelVersion :: Lens.Lens' CreateCollectionResponse (Prelude.Maybe Prelude.Text)
createCollectionResponse_faceModelVersion = Lens.lens (\CreateCollectionResponse' {faceModelVersion} -> faceModelVersion) (\s@CreateCollectionResponse' {} a -> s {faceModelVersion = a} :: CreateCollectionResponse)

-- | HTTP status code indicating the result of the operation.
createCollectionResponse_statusCode :: Lens.Lens' CreateCollectionResponse (Prelude.Maybe Prelude.Natural)
createCollectionResponse_statusCode = Lens.lens (\CreateCollectionResponse' {statusCode} -> statusCode) (\s@CreateCollectionResponse' {} a -> s {statusCode = a} :: CreateCollectionResponse)

-- | The response's http status code.
createCollectionResponse_httpStatus :: Lens.Lens' CreateCollectionResponse Prelude.Int
createCollectionResponse_httpStatus = Lens.lens (\CreateCollectionResponse' {httpStatus} -> httpStatus) (\s@CreateCollectionResponse' {} a -> s {httpStatus = a} :: CreateCollectionResponse)

instance Prelude.NFData CreateCollectionResponse where
  rnf CreateCollectionResponse' {..} =
    Prelude.rnf collectionArn
      `Prelude.seq` Prelude.rnf faceModelVersion
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf httpStatus

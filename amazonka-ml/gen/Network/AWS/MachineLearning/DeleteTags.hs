{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags associated with an ML object. After this
-- operation is complete, you can\'t recover deleted tags.
--
-- If you specify a tag that doesn\'t exist, Amazon ML ignores it.
module Network.AWS.MachineLearning.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_tagKeys,
    deleteTags_resourceId,
    deleteTags_resourceType,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,

    -- * Response Lenses
    deleteTagsResponse_resourceId,
    deleteTagsResponse_resourceType,
    deleteTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | One or more tags to delete.
    tagKeys :: [Prelude.Text],
    -- | The ID of the tagged ML object. For example, @exampleModelId@.
    resourceId :: Prelude.Text,
    -- | The type of the tagged ML object.
    resourceType :: TaggableResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'deleteTags_tagKeys' - One or more tags to delete.
--
-- 'resourceId', 'deleteTags_resourceId' - The ID of the tagged ML object. For example, @exampleModelId@.
--
-- 'resourceType', 'deleteTags_resourceType' - The type of the tagged ML object.
newDeleteTags ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  DeleteTags
newDeleteTags pResourceId_ pResourceType_ =
  DeleteTags'
    { tagKeys = Prelude.mempty,
      resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | One or more tags to delete.
deleteTags_tagKeys :: Lens.Lens' DeleteTags [Prelude.Text]
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Prelude.. Prelude._Coerce

-- | The ID of the tagged ML object. For example, @exampleModelId@.
deleteTags_resourceId :: Lens.Lens' DeleteTags Prelude.Text
deleteTags_resourceId = Lens.lens (\DeleteTags' {resourceId} -> resourceId) (\s@DeleteTags' {} a -> s {resourceId = a} :: DeleteTags)

-- | The type of the tagged ML object.
deleteTags_resourceType :: Lens.Lens' DeleteTags TaggableResourceType
deleteTags_resourceType = Lens.lens (\DeleteTags' {resourceType} -> resourceType) (\s@DeleteTags' {} a -> s {resourceType = a} :: DeleteTags)

instance Prelude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTagsResponse'
            Prelude.<$> (x Prelude..?> "ResourceId")
            Prelude.<*> (x Prelude..?> "ResourceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTags

instance Prelude.NFData DeleteTags

instance Prelude.ToHeaders DeleteTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.DeleteTags" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKeys" Prelude..= tagKeys),
            Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just
              ("ResourceType" Prelude..= resourceType)
          ]
      )

instance Prelude.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTags where
  toQuery = Prelude.const Prelude.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  { -- | The ID of the ML object from which tags were deleted.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the ML object from which tags were deleted.
    resourceType :: Prelude.Maybe TaggableResourceType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'deleteTagsResponse_resourceId' - The ID of the ML object from which tags were deleted.
--
-- 'resourceType', 'deleteTagsResponse_resourceType' - The type of the ML object from which tags were deleted.
--
-- 'httpStatus', 'deleteTagsResponse_httpStatus' - The response's http status code.
newDeleteTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTagsResponse
newDeleteTagsResponse pHttpStatus_ =
  DeleteTagsResponse'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the ML object from which tags were deleted.
deleteTagsResponse_resourceId :: Lens.Lens' DeleteTagsResponse (Prelude.Maybe Prelude.Text)
deleteTagsResponse_resourceId = Lens.lens (\DeleteTagsResponse' {resourceId} -> resourceId) (\s@DeleteTagsResponse' {} a -> s {resourceId = a} :: DeleteTagsResponse)

-- | The type of the ML object from which tags were deleted.
deleteTagsResponse_resourceType :: Lens.Lens' DeleteTagsResponse (Prelude.Maybe TaggableResourceType)
deleteTagsResponse_resourceType = Lens.lens (\DeleteTagsResponse' {resourceType} -> resourceType) (\s@DeleteTagsResponse' {} a -> s {resourceType = a} :: DeleteTagsResponse)

-- | The response's http status code.
deleteTagsResponse_httpStatus :: Lens.Lens' DeleteTagsResponse Prelude.Int
deleteTagsResponse_httpStatus = Lens.lens (\DeleteTagsResponse' {httpStatus} -> httpStatus) (\s@DeleteTagsResponse' {} a -> s {httpStatus = a} :: DeleteTagsResponse)

instance Prelude.NFData DeleteTagsResponse

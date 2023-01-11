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
-- Module      : Amazonka.MachineLearning.DeleteTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags associated with an ML object. After this
-- operation is complete, you can\'t recover deleted tags.
--
-- If you specify a tag that doesn\'t exist, Amazon ML ignores it.
module Amazonka.MachineLearning.DeleteTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | One or more tags to delete.
    tagKeys :: [Prelude.Text],
    -- | The ID of the tagged ML object. For example, @exampleModelId@.
    resourceId :: Prelude.Text,
    -- | The type of the tagged ML object.
    resourceType :: TaggableResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Prelude.. Lens.coerced

-- | The ID of the tagged ML object. For example, @exampleModelId@.
deleteTags_resourceId :: Lens.Lens' DeleteTags Prelude.Text
deleteTags_resourceId = Lens.lens (\DeleteTags' {resourceId} -> resourceId) (\s@DeleteTags' {} a -> s {resourceId = a} :: DeleteTags)

-- | The type of the tagged ML object.
deleteTags_resourceType :: Lens.Lens' DeleteTags TaggableResourceType
deleteTags_resourceType = Lens.lens (\DeleteTags' {resourceType} -> resourceType) (\s@DeleteTags' {} a -> s {resourceType = a} :: DeleteTags)

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTagsResponse'
            Prelude.<$> (x Data..?> "ResourceId")
            Prelude.<*> (x Data..?> "ResourceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTags where
  hashWithSalt _salt DeleteTags' {..} =
    _salt `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData DeleteTags where
  rnf DeleteTags' {..} =
    Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders DeleteTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DeleteTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKeys" Data..= tagKeys),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTags where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTagsResponse where
  rnf DeleteTagsResponse' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf httpStatus

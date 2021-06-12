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
-- Module      : Network.AWS.Discovery.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between configuration items and one or more
-- tags. This API accepts a list of multiple configuration items.
module Network.AWS.Discovery.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_tags,
    deleteTags_configurationIds,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,

    -- * Response Lenses
    deleteTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | Tags that you want to delete from one or more configuration items.
    -- Specify the tags that you want to delete in a /key/-/value/ format. For
    -- example:
    --
    -- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
    tags :: Core.Maybe [Tag],
    -- | A list of configuration items with tags that you want to delete.
    configurationIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'deleteTags_tags' - Tags that you want to delete from one or more configuration items.
-- Specify the tags that you want to delete in a /key/-/value/ format. For
-- example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
--
-- 'configurationIds', 'deleteTags_configurationIds' - A list of configuration items with tags that you want to delete.
newDeleteTags ::
  DeleteTags
newDeleteTags =
  DeleteTags'
    { tags = Core.Nothing,
      configurationIds = Core.mempty
    }

-- | Tags that you want to delete from one or more configuration items.
-- Specify the tags that you want to delete in a /key/-/value/ format. For
-- example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
deleteTags_tags :: Lens.Lens' DeleteTags (Core.Maybe [Tag])
deleteTags_tags = Lens.lens (\DeleteTags' {tags} -> tags) (\s@DeleteTags' {} a -> s {tags = a} :: DeleteTags) Core.. Lens.mapping Lens._Coerce

-- | A list of configuration items with tags that you want to delete.
deleteTags_configurationIds :: Lens.Lens' DeleteTags [Core.Text]
deleteTags_configurationIds = Lens.lens (\DeleteTags' {configurationIds} -> configurationIds) (\s@DeleteTags' {} a -> s {configurationIds = a} :: DeleteTags) Core.. Lens._Coerce

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTags

instance Core.NFData DeleteTags

instance Core.ToHeaders DeleteTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DeleteTags" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just
              ("configurationIds" Core..= configurationIds)
          ]
      )

instance Core.ToPath DeleteTags where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTagsResponse_httpStatus' - The response's http status code.
newDeleteTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTagsResponse
newDeleteTagsResponse pHttpStatus_ =
  DeleteTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTagsResponse_httpStatus :: Lens.Lens' DeleteTagsResponse Core.Int
deleteTagsResponse_httpStatus = Lens.lens (\DeleteTagsResponse' {httpStatus} -> httpStatus) (\s@DeleteTagsResponse' {} a -> s {httpStatus = a} :: DeleteTagsResponse)

instance Core.NFData DeleteTagsResponse

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
-- Module      : Amazonka.DirectoryService.RemoveTagsFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a directory.
module Amazonka.DirectoryService.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,

    -- * Response Lenses
    removeTagsFromResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | Identifier (ID) of the directory from which to remove the tag.
    resourceId :: Prelude.Text,
    -- | The tag key (name) of the tag to be removed.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'removeTagsFromResource_resourceId' - Identifier (ID) of the directory from which to remove the tag.
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - The tag key (name) of the tag to be removed.
newRemoveTagsFromResource ::
  -- | 'resourceId'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceId_ =
  RemoveTagsFromResource'
    { resourceId = pResourceId_,
      tagKeys = Prelude.mempty
    }

-- | Identifier (ID) of the directory from which to remove the tag.
removeTagsFromResource_resourceId :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceId = Lens.lens (\RemoveTagsFromResource' {resourceId} -> resourceId) (\s@RemoveTagsFromResource' {} a -> s {resourceId = a} :: RemoveTagsFromResource)

-- | The tag key (name) of the tag to be removed.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTagsFromResource where
  hashWithSalt _salt RemoveTagsFromResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromResource where
  rnf RemoveTagsFromResource' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.RemoveTagsFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsFromResourceResponse_httpStatus' - The response's http status code.
newRemoveTagsFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse pHttpStatus_ =
  RemoveTagsFromResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
  where
  rnf RemoveTagsFromResourceResponse' {..} =
    Prelude.rnf httpStatus

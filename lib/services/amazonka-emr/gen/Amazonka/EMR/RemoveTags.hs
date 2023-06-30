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
-- Module      : Amazonka.EMR.RemoveTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an Amazon EMR resource, such as a cluster or Amazon
-- EMR Studio. Tags make it easier to associate resources in various ways,
-- such as grouping clusters to track your Amazon EMR resource allocation
-- costs. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
--
-- The following example removes the stack tag with value Prod from a
-- cluster:
module Amazonka.EMR.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_resourceId,
    removeTags_tagKeys,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input identifies an Amazon EMR resource and a list of tags to
-- remove.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The Amazon EMR resource identifier from which tags will be removed. For
    -- example, a cluster identifier or an Amazon EMR Studio ID.
    resourceId :: Prelude.Text,
    -- | A list of tag keys to remove from the resource.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'removeTags_resourceId' - The Amazon EMR resource identifier from which tags will be removed. For
-- example, a cluster identifier or an Amazon EMR Studio ID.
--
-- 'tagKeys', 'removeTags_tagKeys' - A list of tag keys to remove from the resource.
newRemoveTags ::
  -- | 'resourceId'
  Prelude.Text ->
  RemoveTags
newRemoveTags pResourceId_ =
  RemoveTags'
    { resourceId = pResourceId_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon EMR resource identifier from which tags will be removed. For
-- example, a cluster identifier or an Amazon EMR Studio ID.
removeTags_resourceId :: Lens.Lens' RemoveTags Prelude.Text
removeTags_resourceId = Lens.lens (\RemoveTags' {resourceId} -> resourceId) (\s@RemoveTags' {} a -> s {resourceId = a} :: RemoveTags)

-- | A list of tag keys to remove from the resource.
removeTags_tagKeys :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_tagKeys = Lens.lens (\RemoveTags' {tagKeys} -> tagKeys) (\s@RemoveTags' {} a -> s {tagKeys = a} :: RemoveTags) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTags where
  hashWithSalt _salt RemoveTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTags where
  rnf RemoveTags' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.RemoveTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTags where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTags where
  toQuery = Prelude.const Prelude.mempty

-- | This output indicates the result of removing tags from the resource.
--
-- /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsResponse_httpStatus' - The response's http status code.
newRemoveTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsResponse
newRemoveTagsResponse pHttpStatus_ =
  RemoveTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeTagsResponse_httpStatus :: Lens.Lens' RemoveTagsResponse Prelude.Int
removeTagsResponse_httpStatus = Lens.lens (\RemoveTagsResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsResponse' {} a -> s {httpStatus = a} :: RemoveTagsResponse)

instance Prelude.NFData RemoveTagsResponse where
  rnf RemoveTagsResponse' {..} = Prelude.rnf httpStatus

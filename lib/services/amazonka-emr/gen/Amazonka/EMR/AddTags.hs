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
-- Module      : Amazonka.EMR.AddTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an Amazon EMR resource, such as a cluster or an Amazon EMR
-- Studio. Tags make it easier to associate resources in various ways, such
-- as grouping clusters to track your Amazon EMR resource allocation costs.
-- For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
module Amazonka.EMR.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_resourceId,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
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
-- attach.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon EMR resource identifier to which tags will be added. For
    -- example, a cluster identifier or an Amazon EMR Studio ID.
    resourceId :: Prelude.Text,
    -- | A list of tags to associate with a resource. Tags are user-defined
    -- key-value pairs that consist of a required key string with a maximum of
    -- 128 characters, and an optional value string with a maximum of 256
    -- characters.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'addTags_resourceId' - The Amazon EMR resource identifier to which tags will be added. For
-- example, a cluster identifier or an Amazon EMR Studio ID.
--
-- 'tags', 'addTags_tags' - A list of tags to associate with a resource. Tags are user-defined
-- key-value pairs that consist of a required key string with a maximum of
-- 128 characters, and an optional value string with a maximum of 256
-- characters.
newAddTags ::
  -- | 'resourceId'
  Prelude.Text ->
  AddTags
newAddTags pResourceId_ =
  AddTags'
    { resourceId = pResourceId_,
      tags = Prelude.mempty
    }

-- | The Amazon EMR resource identifier to which tags will be added. For
-- example, a cluster identifier or an Amazon EMR Studio ID.
addTags_resourceId :: Lens.Lens' AddTags Prelude.Text
addTags_resourceId = Lens.lens (\AddTags' {resourceId} -> resourceId) (\s@AddTags' {} a -> s {resourceId = a} :: AddTags)

-- | A list of tags to associate with a resource. Tags are user-defined
-- key-value pairs that consist of a required key string with a maximum of
-- 128 characters, and an optional value string with a maximum of 256
-- characters.
addTags_tags :: Lens.Lens' AddTags [Tag]
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Lens.coerced

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags where
  hashWithSalt _salt AddTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTags where
  rnf AddTags' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders AddTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("ElasticMapReduce.AddTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddTags where
  toJSON AddTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath AddTags where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | This output indicates the result of adding tags to a resource.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse where
  rnf AddTagsResponse' {..} = Prelude.rnf httpStatus

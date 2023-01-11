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
-- Module      : Amazonka.MediaStore.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to the specified AWS Elemental MediaStore container. Tags are
-- key:value pairs that you can associate with AWS resources. For example,
-- the tag key might be \"customer\" and the tag value might be
-- \"companyA.\" You can specify one or more tags to add to each container.
-- You can add up to 50 tags to each container. For more information about
-- tagging, including naming and usage conventions, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore>.
module Amazonka.MediaStore.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resource,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) for the container.
    resource :: Prelude.Text,
    -- | An array of key:value pairs that you want to add to the container. You
    -- need to specify only the tags that you want to add or update. For
    -- example, suppose a container already has two tags (customer:CompanyA and
    -- priority:High). You want to change the priority tag and also add a third
    -- tag (type:Contract). For TagResource, you specify the following tags:
    -- priority:Medium, type:Contract. The result is that your container has
    -- three tags: customer:CompanyA, priority:Medium, and type:Contract.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'tagResource_resource' - The Amazon Resource Name (ARN) for the container.
--
-- 'tags', 'tagResource_tags' - An array of key:value pairs that you want to add to the container. You
-- need to specify only the tags that you want to add or update. For
-- example, suppose a container already has two tags (customer:CompanyA and
-- priority:High). You want to change the priority tag and also add a third
-- tag (type:Contract). For TagResource, you specify the following tags:
-- priority:Medium, type:Contract. The result is that your container has
-- three tags: customer:CompanyA, priority:Medium, and type:Contract.
newTagResource ::
  -- | 'resource'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  TagResource
newTagResource pResource_ pTags_ =
  TagResource'
    { resource = pResource_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The Amazon Resource Name (ARN) for the container.
tagResource_resource :: Lens.Lens' TagResource Prelude.Text
tagResource_resource = Lens.lens (\TagResource' {resource} -> resource) (\s@TagResource' {} a -> s {resource = a} :: TagResource)

-- | An array of key:value pairs that you want to add to the container. You
-- need to specify only the tags that you want to add or update. For
-- example, suppose a container already has two tags (customer:CompanyA and
-- priority:High). You want to change the priority tag and also add a third
-- tag (type:Contract). For TagResource, you specify the following tags:
-- priority:Medium, type:Contract. The result is that your container has
-- three tags: customer:CompanyA, priority:Medium, and type:Contract.
tagResource_tags :: Lens.Lens' TagResource (Prelude.NonEmpty Tag)
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource where
  hashWithSalt _salt TagResource' {..} =
    _salt `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf resource `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.TagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TagResource where
  toJSON TagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Resource" Data..= resource),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'tagResourceResponse_httpStatus' - The response's http status code.
newTagResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagResourceResponse
newTagResourceResponse pHttpStatus_ =
  TagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
tagResourceResponse_httpStatus :: Lens.Lens' TagResourceResponse Prelude.Int
tagResourceResponse_httpStatus = Lens.lens (\TagResourceResponse' {httpStatus} -> httpStatus) (\s@TagResourceResponse' {} a -> s {httpStatus = a} :: TagResourceResponse)

instance Prelude.NFData TagResourceResponse where
  rnf TagResourceResponse' {..} = Prelude.rnf httpStatus

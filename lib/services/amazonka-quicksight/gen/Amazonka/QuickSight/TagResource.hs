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
-- Module      : Amazonka.QuickSight.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more tags (key-value pairs) to the specified Amazon
-- QuickSight resource.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions, by granting a user permission to
-- access or change only resources with certain tag values. You can use the
-- @TagResource@ operation with a resource that already has tags. If you
-- specify a new tag key for the resource, this tag is appended to the list
-- of tags associated with the resource. If you specify a tag key that is
-- already associated with the resource, the new tag value that you specify
-- replaces the previous value for that tag.
--
-- You can associate as many as 50 tags with a resource. Amazon QuickSight
-- supports tagging on data set, data source, dashboard, and template.
--
-- Tagging for Amazon QuickSight works in a similar way to tagging for
-- other Amazon Web Services services, except for the following:
--
-- -   You can\'t use tags to track costs for Amazon QuickSight. This
--     isn\'t possible because you can\'t tag the resources that Amazon
--     QuickSight costs are based on, for example Amazon QuickSight storage
--     capacity (SPICE), number of users, type of users, and usage metrics.
--
-- -   Amazon QuickSight doesn\'t currently support the tag editor for
--     Resource Groups.
module Amazonka.QuickSight.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceArn,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_requestId,
    tagResourceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to tag.
    resourceArn :: Prelude.Text,
    -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the resource.
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
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Name (ARN) of the resource that you want to tag.
--
-- 'tags', 'tagResource_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the resource.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  TagResource
newTagResource pResourceArn_ pTags_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The Amazon Resource Name (ARN) of the resource that you want to tag.
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the resource.
tagResource_tags :: Lens.Lens' TagResource (Prelude.NonEmpty Tag)
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource where
  hashWithSalt _salt TagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TagResource where
  toJSON TagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Tags" Data..= tags)]
      )

instance Data.ToPath TagResource where
  toPath TagResource' {..} =
    Prelude.mconcat
      ["/resources/", Data.toBS resourceArn, "/tags"]

instance Data.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'requestId', 'tagResourceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'tagResourceResponse_status' - The HTTP status of the request.
newTagResourceResponse ::
  -- | 'status'
  Prelude.Int ->
  TagResourceResponse
newTagResourceResponse pStatus_ =
  TagResourceResponse'
    { requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
tagResourceResponse_requestId :: Lens.Lens' TagResourceResponse (Prelude.Maybe Prelude.Text)
tagResourceResponse_requestId = Lens.lens (\TagResourceResponse' {requestId} -> requestId) (\s@TagResourceResponse' {} a -> s {requestId = a} :: TagResourceResponse)

-- | The HTTP status of the request.
tagResourceResponse_status :: Lens.Lens' TagResourceResponse Prelude.Int
tagResourceResponse_status = Lens.lens (\TagResourceResponse' {status} -> status) (\s@TagResourceResponse' {} a -> s {status = a} :: TagResourceResponse)

instance Prelude.NFData TagResourceResponse where
  rnf TagResourceResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

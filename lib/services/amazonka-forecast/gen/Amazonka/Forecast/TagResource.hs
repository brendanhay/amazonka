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
-- Module      : Amazonka.Forecast.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified tags to a resource with the specified
-- @resourceArn@. If existing tags on a resource are not specified in the
-- request parameters, they are not changed. When a resource is deleted,
-- the tags associated with that resource are also deleted.
module Amazonka.Forecast.TagResource
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
    tagResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) that identifies the resource for which to
    -- list the tags.
    resourceArn :: Prelude.Text,
    -- | The tags to add to the resource. A tag is an array of key-value pairs.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50.
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8.
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for keys as it is reserved for Amazon Web Services
    --     use. You cannot edit or delete tag keys with this prefix. Values can
    --     have this prefix. If a tag value has @aws@ as its prefix but the key
    --     does not, then Forecast considers it to be a user tag and will count
    --     against the limit of 50 tags. Tags with only the key prefix of @aws@
    --     do not count against your tags per resource limit.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags.
--
-- 'tags', 'tagResource_tags' - The tags to add to the resource. A tag is an array of key-value pairs.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for Amazon Web Services
--     use. You cannot edit or delete tag keys with this prefix. Values can
--     have this prefix. If a tag value has @aws@ as its prefix but the key
--     does not, then Forecast considers it to be a user tag and will count
--     against the limit of 50 tags. Tags with only the key prefix of @aws@
--     do not count against your tags per resource limit.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags.
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | The tags to add to the resource. A tag is an array of key-value pairs.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for Amazon Web Services
--     use. You cannot edit or delete tag keys with this prefix. Values can
--     have this prefix. If a tag value has @aws@ as its prefix but the key
--     does not, then Forecast considers it to be a user tag and will count
--     against the limit of 50 tags. Tags with only the key prefix of @aws@
--     do not count against your tags per resource limit.
tagResource_tags :: Lens.Lens' TagResource [Tag]
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
          [ "X-Amz-Target"
              Data.=# ("AmazonForecast.TagResource" :: Prelude.ByteString),
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
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
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

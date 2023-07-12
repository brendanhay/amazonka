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
-- Module      : Amazonka.Location.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more tags (key-value pairs) to the specified Amazon
-- Location Service resource.
--
-- >  <p>Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.</p> <p>You can use the <code>TagResource</code> operation with an Amazon Location Service resource that already has tags. If you specify a new tag key for the resource, this tag is appended to the tags already associated with the resource. If you specify a tag key that's already associated with the resource, the new tag value that you specify replaces the previous value for that tag. </p> <p>You can associate up to 50 tags with a resource.</p>
module Amazonka.Location.TagResource
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
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) of the resource whose tags you want to
    -- update.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:resourcetype\/ExampleResource@
    resourceArn :: Prelude.Text,
    -- | Applies one or more tags to specific resource. A tag is a key-value pair
    -- that helps you manage, identify, search, and filter your resources.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource.
    --
    -- -   Each tag key must be unique and must have exactly one associated
    --     value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@
    --
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
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
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Name (ARN) of the resource whose tags you want to
-- update.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:resourcetype\/ExampleResource@
--
-- 'tags', 'tagResource_tags' - Applies one or more tags to specific resource. A tag is a key-value pair
-- that helps you manage, identify, search, and filter your resources.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource.
--
-- -   Each tag key must be unique and must have exactly one associated
--     value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@
--
-- -   Cannot use \"aws:\" as a prefix for a key.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to
-- update.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:resourcetype\/ExampleResource@
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | Applies one or more tags to specific resource. A tag is a key-value pair
-- that helps you manage, identify, search, and filter your resources.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource.
--
-- -   Each tag key must be unique and must have exactly one associated
--     value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@
--
-- -   Cannot use \"aws:\" as a prefix for a key.
tagResource_tags :: Lens.Lens' TagResource (Prelude.HashMap Prelude.Text Prelude.Text)
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
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
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
    Prelude.mconcat ["/tags/", Data.toBS resourceArn]

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

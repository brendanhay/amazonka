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
-- Module      : Amazonka.CostExplorer.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An API operation for adding one or more tags (key-value pairs) to a
-- resource.
--
-- You can use the @TagResource@ operation with a resource that already has
-- tags. If you specify a new tag key for the resource, this tag is
-- appended to the list of tags associated with the resource. If you
-- specify a tag key that is already associated with the resource, the new
-- tag value you specify replaces the previous value for that tag.
--
-- Although the maximum number of array members is 200, user-tag maximum is
-- 50. The remaining are reserved for Amazon Web Services use.
module Amazonka.CostExplorer.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceArn,
    tagResource_resourceTags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) of the resource. For a list of supported
    -- resources, see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_ResourceTag.html ResourceTag>.
    resourceArn :: Prelude.Text,
    -- | A list of tag key-value pairs to be added to the resource.
    --
    -- Each tag consists of a key and a value, and each key must be unique for
    -- the resource. The following restrictions apply to resource tags:
    --
    -- -   Although the maximum number of array members is 200, you can assign
    --     a maximum of 50 user-tags to one resource. The remaining are
    --     reserved for Amazon Web Services use
    --
    -- -   The maximum length of a key is 128 characters
    --
    -- -   The maximum length of a value is 256 characters
    --
    -- -   Keys and values can only contain alphanumeric characters, spaces,
    --     and any of the following: @_.:\/=+\@-@
    --
    -- -   Keys and values are case sensitive
    --
    -- -   Keys and values are trimmed for any leading or trailing whitespaces
    --
    -- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
    --     for Amazon Web Services use
    resourceTags :: [ResourceTag]
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
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Name (ARN) of the resource. For a list of supported
-- resources, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_ResourceTag.html ResourceTag>.
--
-- 'resourceTags', 'tagResource_resourceTags' - A list of tag key-value pairs to be added to the resource.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      resourceTags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource. For a list of supported
-- resources, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_ResourceTag.html ResourceTag>.
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | A list of tag key-value pairs to be added to the resource.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
tagResource_resourceTags :: Lens.Lens' TagResource [ResourceTag]
tagResource_resourceTags = Lens.lens (\TagResource' {resourceTags} -> resourceTags) (\s@TagResource' {} a -> s {resourceTags = a} :: TagResource) Prelude.. Lens.coerced

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
      `Prelude.hashWithSalt` resourceTags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceTags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.TagResource" ::
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
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("ResourceTags" Data..= resourceTags)
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

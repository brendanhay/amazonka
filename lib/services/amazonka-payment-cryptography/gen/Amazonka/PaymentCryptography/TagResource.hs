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
-- Module      : Amazonka.PaymentCryptography.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or edits tags on an Amazon Web Services Payment Cryptography key.
--
-- Tagging or untagging an Amazon Web Services Payment Cryptography key can
-- allow or deny permission to the key.
--
-- Each tag consists of a tag key and a tag value, both of which are
-- case-sensitive strings. The tag value can be an empty (null) string. To
-- add a tag, specify a new tag key and a tag value. To edit a tag, specify
-- an existing tag key and a new tag value. You can also add tags to an
-- Amazon Web Services Payment Cryptography key when you create it with
-- CreateKey.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   ListTagsForResource
--
-- -   UntagResource
module Amazonka.PaymentCryptography.TagResource
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
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The @KeyARN@ of the key whose tags are being updated.
    resourceArn :: Prelude.Text,
    -- | One or more tags. Each tag consists of a tag key and a tag value. The
    -- tag value can be an empty (null) string. You can\'t have more than one
    -- tag on an Amazon Web Services Payment Cryptography key with the same tag
    -- key. If you specify an existing tag key with a different tag value,
    -- Amazon Web Services Payment Cryptography replaces the current tag value
    -- with the new one.
    --
    -- Don\'t include confidential or sensitive information in this field. This
    -- field may be displayed in plaintext in CloudTrail logs and other output.
    --
    -- To use this parameter, you must have TagResource permission in an IAM
    -- policy.
    --
    -- Don\'t include confidential or sensitive information in this field. This
    -- field may be displayed in plaintext in CloudTrail logs and other output.
    tags :: [Tag]
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
-- 'resourceArn', 'tagResource_resourceArn' - The @KeyARN@ of the key whose tags are being updated.
--
-- 'tags', 'tagResource_tags' - One or more tags. Each tag consists of a tag key and a tag value. The
-- tag value can be an empty (null) string. You can\'t have more than one
-- tag on an Amazon Web Services Payment Cryptography key with the same tag
-- key. If you specify an existing tag key with a different tag value,
-- Amazon Web Services Payment Cryptography replaces the current tag value
-- with the new one.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
--
-- To use this parameter, you must have TagResource permission in an IAM
-- policy.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The @KeyARN@ of the key whose tags are being updated.
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | One or more tags. Each tag consists of a tag key and a tag value. The
-- tag value can be an empty (null) string. You can\'t have more than one
-- tag on an Amazon Web Services Payment Cryptography key with the same tag
-- key. If you specify an existing tag key with a different tag value,
-- Amazon Web Services Payment Cryptography replaces the current tag value
-- with the new one.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
--
-- To use this parameter, you must have TagResource permission in an IAM
-- policy.
--
-- Don\'t include confidential or sensitive information in this field. This
-- field may be displayed in plaintext in CloudTrail logs and other output.
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
              Data.=# ( "PaymentCryptographyControlPlane.TagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
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

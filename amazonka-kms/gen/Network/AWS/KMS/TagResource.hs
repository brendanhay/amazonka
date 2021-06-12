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
-- Module      : Network.AWS.KMS.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or edits tags on a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>.
--
-- Each tag consists of a tag key and a tag value, both of which are
-- case-sensitive strings. The tag value can be an empty (null) string.
--
-- To add a tag, specify a new tag key and a tag value. To edit a tag,
-- specify an existing tag key and a new tag value.
--
-- You can use this operation to tag a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>,
-- but you cannot tag an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK>,
-- an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-owned-cmk AWS owned CMK>,
-- or an alias.
--
-- For general information about tags, including the format and syntax, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /Amazon Web Services General Reference/. For information about
-- using tags in AWS KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging keys>.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- (key policy)
--
-- __Related operations__
--
-- -   UntagResource
--
-- -   ListResourceTags
module Network.AWS.KMS.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_keyId,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | Identifies a customer managed CMK in the account and Region.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Core.Text,
    -- | One or more tags.
    --
    -- Each tag consists of a tag key and a tag value. The tag value can be an
    -- empty (null) string.
    --
    -- You cannot have more than one tag on a CMK with the same tag key. If you
    -- specify an existing tag key with a different tag value, AWS KMS replaces
    -- the current tag value with the specified one.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'tagResource_keyId' - Identifies a customer managed CMK in the account and Region.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
--
-- 'tags', 'tagResource_tags' - One or more tags.
--
-- Each tag consists of a tag key and a tag value. The tag value can be an
-- empty (null) string.
--
-- You cannot have more than one tag on a CMK with the same tag key. If you
-- specify an existing tag key with a different tag value, AWS KMS replaces
-- the current tag value with the specified one.
newTagResource ::
  -- | 'keyId'
  Core.Text ->
  TagResource
newTagResource pKeyId_ =
  TagResource' {keyId = pKeyId_, tags = Core.mempty}

-- | Identifies a customer managed CMK in the account and Region.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
tagResource_keyId :: Lens.Lens' TagResource Core.Text
tagResource_keyId = Lens.lens (\TagResource' {keyId} -> keyId) (\s@TagResource' {} a -> s {keyId = a} :: TagResource)

-- | One or more tags.
--
-- Each tag consists of a tag key and a tag value. The tag value can be an
-- empty (null) string.
--
-- You cannot have more than one tag on a CMK with the same tag key. If you
-- specify an existing tag key with a different tag value, AWS KMS replaces
-- the current tag value with the specified one.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Core.. Lens._Coerce

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Core.Hashable TagResource

instance Core.NFData TagResource

instance Core.ToHeaders TagResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.TagResource" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TagResource where
  toJSON TagResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagResource where
  toPath = Core.const "/"

instance Core.ToQuery TagResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Core.NFData TagResourceResponse

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
-- Module      : Amazonka.KMS.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or edits tags on a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
-- in the /Key Management Service Developer Guide/.
--
-- Each tag consists of a tag key and a tag value, both of which are
-- case-sensitive strings. The tag value can be an empty (null) string. To
-- add a tag, specify a new tag key and a tag value. To edit a tag, specify
-- an existing tag key and a new tag value.
--
-- You can use this operation to tag a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>,
-- but you cannot tag an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed key>,
-- an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-owned-cmk Amazon Web Services owned key>,
-- a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#keystore-concept custom key store>,
-- or an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#alias-concept alias>.
--
-- You can also add tags to a KMS key while creating it (CreateKey) or
-- replicating it (ReplicateKey).
--
-- For information about using tags in KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging keys>.
-- For general information about tags, including the format and syntax, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:TagResource>
-- (key policy)
--
-- __Related operations__
--
-- -   CreateKey
--
-- -   ListResourceTags
--
-- -   ReplicateKey
--
-- -   UntagResource
module Amazonka.KMS.TagResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | Identifies a customer managed key in the account and Region.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | One or more tags.
    --
    -- Each tag consists of a tag key and a tag value. The tag value can be an
    -- empty (null) string.
    --
    -- You cannot have more than one tag on a KMS key with the same tag key. If
    -- you specify an existing tag key with a different tag value, KMS replaces
    -- the current tag value with the specified one.
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
-- 'keyId', 'tagResource_keyId' - Identifies a customer managed key in the account and Region.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'tags', 'tagResource_tags' - One or more tags.
--
-- Each tag consists of a tag key and a tag value. The tag value can be an
-- empty (null) string.
--
-- You cannot have more than one tag on a KMS key with the same tag key. If
-- you specify an existing tag key with a different tag value, KMS replaces
-- the current tag value with the specified one.
newTagResource ::
  -- | 'keyId'
  Prelude.Text ->
  TagResource
newTagResource pKeyId_ =
  TagResource'
    { keyId = pKeyId_,
      tags = Prelude.mempty
    }

-- | Identifies a customer managed key in the account and Region.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
tagResource_keyId :: Lens.Lens' TagResource Prelude.Text
tagResource_keyId = Lens.lens (\TagResource' {keyId} -> keyId) (\s@TagResource' {} a -> s {keyId = a} :: TagResource)

-- | One or more tags.
--
-- Each tag consists of a tag key and a tag value. The tag value can be an
-- empty (null) string.
--
-- You cannot have more than one tag on a KMS key with the same tag key. If
-- you specify an existing tag key with a different tag value, KMS replaces
-- the current tag value with the specified one.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource where
  hashWithSalt _salt TagResource' {..} =
    _salt `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf keyId `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.TagResource" :: Prelude.ByteString),
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
          [ Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse where
  rnf _ = ()

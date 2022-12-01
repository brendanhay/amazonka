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
-- Module      : Amazonka.KMS.ListResourceTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tags on the specified KMS key.
--
-- For general information about tags, including the format and syntax, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/. For information about
-- using tags in KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/tagging-keys.html Tagging keys>.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListResourceTags>
-- (key policy)
--
-- __Related operations:__
--
-- -   CreateKey
--
-- -   ReplicateKey
--
-- -   TagResource
--
-- -   UntagResource
--
-- This operation returns paginated results.
module Amazonka.KMS.ListResourceTags
  ( -- * Creating a Request
    ListResourceTags (..),
    newListResourceTags,

    -- * Request Lenses
    listResourceTags_marker,
    listResourceTags_limit,
    listResourceTags_keyId,

    -- * Destructuring the Response
    ListResourceTagsResponse (..),
    newListResourceTagsResponse,

    -- * Response Lenses
    listResourceTagsResponse_tags,
    listResourceTagsResponse_truncated,
    listResourceTagsResponse_nextMarker,
    listResourceTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceTags' smart constructor.
data ListResourceTags = ListResourceTags'
  { -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    --
    -- Do not attempt to construct this value. Use only the value of
    -- @NextMarker@ from the truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 50, inclusive. If you do not include a value, it defaults to 50.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Gets tags on the specified KMS key.
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
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listResourceTags_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of
-- @NextMarker@ from the truncated response you just received.
--
-- 'limit', 'listResourceTags_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 50, inclusive. If you do not include a value, it defaults to 50.
--
-- 'keyId', 'listResourceTags_keyId' - Gets tags on the specified KMS key.
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
newListResourceTags ::
  -- | 'keyId'
  Prelude.Text ->
  ListResourceTags
newListResourceTags pKeyId_ =
  ListResourceTags'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of
-- @NextMarker@ from the truncated response you just received.
listResourceTags_marker :: Lens.Lens' ListResourceTags (Prelude.Maybe Prelude.Text)
listResourceTags_marker = Lens.lens (\ListResourceTags' {marker} -> marker) (\s@ListResourceTags' {} a -> s {marker = a} :: ListResourceTags)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 50, inclusive. If you do not include a value, it defaults to 50.
listResourceTags_limit :: Lens.Lens' ListResourceTags (Prelude.Maybe Prelude.Natural)
listResourceTags_limit = Lens.lens (\ListResourceTags' {limit} -> limit) (\s@ListResourceTags' {} a -> s {limit = a} :: ListResourceTags)

-- | Gets tags on the specified KMS key.
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
listResourceTags_keyId :: Lens.Lens' ListResourceTags Prelude.Text
listResourceTags_keyId = Lens.lens (\ListResourceTags' {keyId} -> keyId) (\s@ListResourceTags' {} a -> s {keyId = a} :: ListResourceTags)

instance Core.AWSPager ListResourceTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceTagsResponse_truncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listResourceTagsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceTags_marker
          Lens..~ rs
          Lens.^? listResourceTagsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceTags where
  type
    AWSResponse ListResourceTags =
      ListResourceTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceTagsResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Truncated")
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceTags where
  hashWithSalt _salt ListResourceTags' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData ListResourceTags where
  rnf ListResourceTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf keyId

instance Core.ToHeaders ListResourceTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.ListResourceTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceTags where
  toJSON ListResourceTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.ToPath ListResourceTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourceTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceTagsResponse' smart constructor.
data ListResourceTagsResponse = ListResourceTagsResponse'
  { -- | A list of tags. Each tag consists of a tag key and a tag value.
    --
    -- Tagging or untagging a KMS key can allow or deny permission to the KMS
    -- key. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
    -- in the /Key Management Service Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    --
    -- Do not assume or infer any information from this value.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listResourceTagsResponse_tags' - A list of tags. Each tag consists of a tag key and a tag value.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- 'truncated', 'listResourceTagsResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'nextMarker', 'listResourceTagsResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
--
-- 'httpStatus', 'listResourceTagsResponse_httpStatus' - The response's http status code.
newListResourceTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceTagsResponse
newListResourceTagsResponse pHttpStatus_ =
  ListResourceTagsResponse'
    { tags = Prelude.Nothing,
      truncated = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tags. Each tag consists of a tag key and a tag value.
--
-- Tagging or untagging a KMS key can allow or deny permission to the KMS
-- key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
-- in the /Key Management Service Developer Guide/.
listResourceTagsResponse_tags :: Lens.Lens' ListResourceTagsResponse (Prelude.Maybe [Tag])
listResourceTagsResponse_tags = Lens.lens (\ListResourceTagsResponse' {tags} -> tags) (\s@ListResourceTagsResponse' {} a -> s {tags = a} :: ListResourceTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listResourceTagsResponse_truncated :: Lens.Lens' ListResourceTagsResponse (Prelude.Maybe Prelude.Bool)
listResourceTagsResponse_truncated = Lens.lens (\ListResourceTagsResponse' {truncated} -> truncated) (\s@ListResourceTagsResponse' {} a -> s {truncated = a} :: ListResourceTagsResponse)

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
listResourceTagsResponse_nextMarker :: Lens.Lens' ListResourceTagsResponse (Prelude.Maybe Prelude.Text)
listResourceTagsResponse_nextMarker = Lens.lens (\ListResourceTagsResponse' {nextMarker} -> nextMarker) (\s@ListResourceTagsResponse' {} a -> s {nextMarker = a} :: ListResourceTagsResponse)

-- | The response's http status code.
listResourceTagsResponse_httpStatus :: Lens.Lens' ListResourceTagsResponse Prelude.Int
listResourceTagsResponse_httpStatus = Lens.lens (\ListResourceTagsResponse' {httpStatus} -> httpStatus) (\s@ListResourceTagsResponse' {} a -> s {httpStatus = a} :: ListResourceTagsResponse)

instance Prelude.NFData ListResourceTagsResponse where
  rnf ListResourceTagsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the names of the key policies that are attached to a KMS key. This
-- operation is designed to get policy names that you can use in a
-- GetKeyPolicy operation. However, the only valid policy name is
-- @default@.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListKeyPolicies>
-- (key policy)
--
-- __Related operations:__
--
-- -   GetKeyPolicy
--
-- -   PutKeyPolicy
--
-- This operation returns paginated results.
module Amazonka.KMS.ListKeyPolicies
  ( -- * Creating a Request
    ListKeyPolicies (..),
    newListKeyPolicies,

    -- * Request Lenses
    listKeyPolicies_limit,
    listKeyPolicies_marker,
    listKeyPolicies_keyId,

    -- * Destructuring the Response
    ListKeyPoliciesResponse (..),
    newListKeyPoliciesResponse,

    -- * Response Lenses
    listKeyPoliciesResponse_nextMarker,
    listKeyPoliciesResponse_policyNames,
    listKeyPoliciesResponse_truncated,
    listKeyPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeyPolicies' smart constructor.
data ListKeyPolicies = ListKeyPolicies'
  { -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 1000, inclusive. If you do not include a value, it defaults to 100.
    --
    -- Only one policy can be attached to a key.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Gets the names of key policies for the specified KMS key.
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
-- Create a value of 'ListKeyPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listKeyPolicies_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- Only one policy can be attached to a key.
--
-- 'marker', 'listKeyPolicies_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- 'keyId', 'listKeyPolicies_keyId' - Gets the names of key policies for the specified KMS key.
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
newListKeyPolicies ::
  -- | 'keyId'
  Prelude.Text ->
  ListKeyPolicies
newListKeyPolicies pKeyId_ =
  ListKeyPolicies'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- Only one policy can be attached to a key.
listKeyPolicies_limit :: Lens.Lens' ListKeyPolicies (Prelude.Maybe Prelude.Natural)
listKeyPolicies_limit = Lens.lens (\ListKeyPolicies' {limit} -> limit) (\s@ListKeyPolicies' {} a -> s {limit = a} :: ListKeyPolicies)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listKeyPolicies_marker :: Lens.Lens' ListKeyPolicies (Prelude.Maybe Prelude.Text)
listKeyPolicies_marker = Lens.lens (\ListKeyPolicies' {marker} -> marker) (\s@ListKeyPolicies' {} a -> s {marker = a} :: ListKeyPolicies)

-- | Gets the names of key policies for the specified KMS key.
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
listKeyPolicies_keyId :: Lens.Lens' ListKeyPolicies Prelude.Text
listKeyPolicies_keyId = Lens.lens (\ListKeyPolicies' {keyId} -> keyId) (\s@ListKeyPolicies' {} a -> s {keyId = a} :: ListKeyPolicies)

instance Core.AWSPager ListKeyPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeyPoliciesResponse_truncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listKeyPoliciesResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listKeyPolicies_marker
          Lens..~ rs
          Lens.^? listKeyPoliciesResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListKeyPolicies where
  type
    AWSResponse ListKeyPolicies =
      ListKeyPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyPoliciesResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "PolicyNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Truncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyPolicies where
  hashWithSalt _salt ListKeyPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData ListKeyPolicies where
  rnf ListKeyPolicies' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders ListKeyPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.ListKeyPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeyPolicies where
  toJSON ListKeyPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("KeyId" Data..= keyId)
          ]
      )

instance Data.ToPath ListKeyPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListKeyPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeyPoliciesResponse' smart constructor.
data ListKeyPoliciesResponse = ListKeyPoliciesResponse'
  { -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of key policy names. The only valid value is @default@.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listKeyPoliciesResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'policyNames', 'listKeyPoliciesResponse_policyNames' - A list of key policy names. The only valid value is @default@.
--
-- 'truncated', 'listKeyPoliciesResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'listKeyPoliciesResponse_httpStatus' - The response's http status code.
newListKeyPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeyPoliciesResponse
newListKeyPoliciesResponse pHttpStatus_ =
  ListKeyPoliciesResponse'
    { nextMarker =
        Prelude.Nothing,
      policyNames = Prelude.Nothing,
      truncated = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listKeyPoliciesResponse_nextMarker :: Lens.Lens' ListKeyPoliciesResponse (Prelude.Maybe Prelude.Text)
listKeyPoliciesResponse_nextMarker = Lens.lens (\ListKeyPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListKeyPoliciesResponse' {} a -> s {nextMarker = a} :: ListKeyPoliciesResponse)

-- | A list of key policy names. The only valid value is @default@.
listKeyPoliciesResponse_policyNames :: Lens.Lens' ListKeyPoliciesResponse (Prelude.Maybe [Prelude.Text])
listKeyPoliciesResponse_policyNames = Lens.lens (\ListKeyPoliciesResponse' {policyNames} -> policyNames) (\s@ListKeyPoliciesResponse' {} a -> s {policyNames = a} :: ListKeyPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listKeyPoliciesResponse_truncated :: Lens.Lens' ListKeyPoliciesResponse (Prelude.Maybe Prelude.Bool)
listKeyPoliciesResponse_truncated = Lens.lens (\ListKeyPoliciesResponse' {truncated} -> truncated) (\s@ListKeyPoliciesResponse' {} a -> s {truncated = a} :: ListKeyPoliciesResponse)

-- | The response's http status code.
listKeyPoliciesResponse_httpStatus :: Lens.Lens' ListKeyPoliciesResponse Prelude.Int
listKeyPoliciesResponse_httpStatus = Lens.lens (\ListKeyPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListKeyPoliciesResponse' {} a -> s {httpStatus = a} :: ListKeyPoliciesResponse)

instance Prelude.NFData ListKeyPoliciesResponse where
  rnf ListKeyPoliciesResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf policyNames
      `Prelude.seq` Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.KMS.ListGrants
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all grants for the specified KMS key.
--
-- You must specify the KMS key in all requests. You can filter the grant
-- list by grant ID or grantee principal.
--
-- For detailed information about grants, including grant terminology, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants in KMS>
-- in the //Key Management Service Developer Guide// . For examples of
-- working with grants in several programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-grants.html Programming grants>.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an Amazon Web
-- Services service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
--
-- __Cross-account use__: Yes. To perform this operation on a KMS key in a
-- different Amazon Web Services account, specify the key ARN in the value
-- of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListGrants>
-- (key policy)
--
-- __Related operations:__
--
-- -   CreateGrant
--
-- -   ListRetirableGrants
--
-- -   RetireGrant
--
-- -   RevokeGrant
--
-- This operation returns paginated results.
module Amazonka.KMS.ListGrants
  ( -- * Creating a Request
    ListGrants (..),
    newListGrants,

    -- * Request Lenses
    listGrants_grantId,
    listGrants_granteePrincipal,
    listGrants_limit,
    listGrants_marker,
    listGrants_keyId,

    -- * Destructuring the Response
    ListGrantsResponse (..),
    newListGrantsResponse,

    -- * Response Lenses
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
    listGrantsResponse_truncated,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGrants' smart constructor.
data ListGrants = ListGrants'
  { -- | Returns only the grant with the specified grant ID. The grant ID
    -- uniquely identifies the grant.
    grantId :: Prelude.Maybe Prelude.Text,
    -- | Returns only grants where the specified principal is the grantee
    -- principal for the grant.
    granteePrincipal :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 100, inclusive. If you do not include a value, it defaults to 50.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Returns only grants for the specified KMS key. This parameter is
    -- required.
    --
    -- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
    -- different Amazon Web Services account, you must use the key ARN.
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
-- Create a value of 'ListGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantId', 'listGrants_grantId' - Returns only the grant with the specified grant ID. The grant ID
-- uniquely identifies the grant.
--
-- 'granteePrincipal', 'listGrants_granteePrincipal' - Returns only grants where the specified principal is the grantee
-- principal for the grant.
--
-- 'limit', 'listGrants_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
--
-- 'marker', 'listGrants_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- 'keyId', 'listGrants_keyId' - Returns only grants for the specified KMS key. This parameter is
-- required.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
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
newListGrants ::
  -- | 'keyId'
  Prelude.Text ->
  ListGrants
newListGrants pKeyId_ =
  ListGrants'
    { grantId = Prelude.Nothing,
      granteePrincipal = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      keyId = pKeyId_
    }

-- | Returns only the grant with the specified grant ID. The grant ID
-- uniquely identifies the grant.
listGrants_grantId :: Lens.Lens' ListGrants (Prelude.Maybe Prelude.Text)
listGrants_grantId = Lens.lens (\ListGrants' {grantId} -> grantId) (\s@ListGrants' {} a -> s {grantId = a} :: ListGrants)

-- | Returns only grants where the specified principal is the grantee
-- principal for the grant.
listGrants_granteePrincipal :: Lens.Lens' ListGrants (Prelude.Maybe Prelude.Text)
listGrants_granteePrincipal = Lens.lens (\ListGrants' {granteePrincipal} -> granteePrincipal) (\s@ListGrants' {} a -> s {granteePrincipal = a} :: ListGrants)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
listGrants_limit :: Lens.Lens' ListGrants (Prelude.Maybe Prelude.Natural)
listGrants_limit = Lens.lens (\ListGrants' {limit} -> limit) (\s@ListGrants' {} a -> s {limit = a} :: ListGrants)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listGrants_marker :: Lens.Lens' ListGrants (Prelude.Maybe Prelude.Text)
listGrants_marker = Lens.lens (\ListGrants' {marker} -> marker) (\s@ListGrants' {} a -> s {marker = a} :: ListGrants)

-- | Returns only grants for the specified KMS key. This parameter is
-- required.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
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
listGrants_keyId :: Lens.Lens' ListGrants Prelude.Text
listGrants_keyId = Lens.lens (\ListGrants' {keyId} -> keyId) (\s@ListGrants' {} a -> s {keyId = a} :: ListGrants)

instance Core.AWSPager ListGrants where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGrantsResponse_truncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listGrantsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listGrants_marker
              Lens..~ rs
              Lens.^? listGrantsResponse_nextMarker
              Prelude.. Lens._Just

instance Core.AWSRequest ListGrants where
  type AWSResponse ListGrants = ListGrantsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ListGrants where
  hashWithSalt _salt ListGrants' {..} =
    _salt
      `Prelude.hashWithSalt` grantId
      `Prelude.hashWithSalt` granteePrincipal
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData ListGrants where
  rnf ListGrants' {..} =
    Prelude.rnf grantId `Prelude.seq`
      Prelude.rnf granteePrincipal `Prelude.seq`
        Prelude.rnf limit `Prelude.seq`
          Prelude.rnf marker `Prelude.seq`
            Prelude.rnf keyId

instance Data.ToHeaders ListGrants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.ListGrants" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGrants where
  toJSON ListGrants' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantId" Data..=) Prelude.<$> grantId,
            ("GranteePrincipal" Data..=)
              Prelude.<$> granteePrincipal,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("KeyId" Data..= keyId)
          ]
      )

instance Data.ToPath ListGrants where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGrants where
  toQuery = Prelude.const Prelude.mempty

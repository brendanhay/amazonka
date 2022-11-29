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
-- Module      : Amazonka.KMS.ListRetirableGrants
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all grants in the Amazon Web Services account
-- and Region that have the specified retiring principal.
--
-- You can specify any principal in your Amazon Web Services account. The
-- grants that are returned include grants for KMS keys in your Amazon Web
-- Services account and other Amazon Web Services accounts. You might use
-- this operation to determine which grants you may retire. To retire a
-- grant, use the RetireGrant operation.
--
-- For detailed information about grants, including grant terminology, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants in KMS>
-- in the //Key Management Service Developer Guide// . For examples of
-- working with grants in several programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-grants.html Programming grants>.
--
-- __Cross-account use__: You must specify a principal in your Amazon Web
-- Services account. However, this operation can return grants in any
-- Amazon Web Services account. You do not need @kms:ListRetirableGrants@
-- permission (or any other additional permission) in any Amazon Web
-- Services account other than your own.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListRetirableGrants>
-- (IAM policy) in your Amazon Web Services account.
--
-- __Related operations:__
--
-- -   CreateGrant
--
-- -   ListGrants
--
-- -   RetireGrant
--
-- -   RevokeGrant
--
-- This operation returns paginated results.
module Amazonka.KMS.ListRetirableGrants
  ( -- * Creating a Request
    ListRetirableGrants (..),
    newListRetirableGrants,

    -- * Request Lenses
    listRetirableGrants_marker,
    listRetirableGrants_limit,
    listRetirableGrants_retiringPrincipal,

    -- * Destructuring the Response
    ListGrantsResponse (..),
    newListGrantsResponse,

    -- * Response Lenses
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRetirableGrants' smart constructor.
data ListRetirableGrants = ListRetirableGrants'
  { -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 100, inclusive. If you do not include a value, it defaults to 50.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The retiring principal for which to list grants. Enter a principal in
    -- your Amazon Web Services account.
    --
    -- To specify the retiring principal, use the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an Amazon Web Services principal. Valid Amazon Web Services
    -- principals include Amazon Web Services accounts (root), IAM users,
    -- federated users, and assumed role users. For examples of the ARN syntax
    -- for specifying a principal, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
    -- in the Example ARNs section of the /Amazon Web Services General
    -- Reference/.
    retiringPrincipal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRetirableGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listRetirableGrants_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- 'limit', 'listRetirableGrants_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
--
-- 'retiringPrincipal', 'listRetirableGrants_retiringPrincipal' - The retiring principal for which to list grants. Enter a principal in
-- your Amazon Web Services account.
--
-- To specify the retiring principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users,
-- federated users, and assumed role users. For examples of the ARN syntax
-- for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
newListRetirableGrants ::
  -- | 'retiringPrincipal'
  Prelude.Text ->
  ListRetirableGrants
newListRetirableGrants pRetiringPrincipal_ =
  ListRetirableGrants'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      retiringPrincipal = pRetiringPrincipal_
    }

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listRetirableGrants_marker :: Lens.Lens' ListRetirableGrants (Prelude.Maybe Prelude.Text)
listRetirableGrants_marker = Lens.lens (\ListRetirableGrants' {marker} -> marker) (\s@ListRetirableGrants' {} a -> s {marker = a} :: ListRetirableGrants)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
listRetirableGrants_limit :: Lens.Lens' ListRetirableGrants (Prelude.Maybe Prelude.Natural)
listRetirableGrants_limit = Lens.lens (\ListRetirableGrants' {limit} -> limit) (\s@ListRetirableGrants' {} a -> s {limit = a} :: ListRetirableGrants)

-- | The retiring principal for which to list grants. Enter a principal in
-- your Amazon Web Services account.
--
-- To specify the retiring principal, use the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an Amazon Web Services principal. Valid Amazon Web Services
-- principals include Amazon Web Services accounts (root), IAM users,
-- federated users, and assumed role users. For examples of the ARN syntax
-- for specifying a principal, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam Amazon Web Services Identity and Access Management (IAM)>
-- in the Example ARNs section of the /Amazon Web Services General
-- Reference/.
listRetirableGrants_retiringPrincipal :: Lens.Lens' ListRetirableGrants Prelude.Text
listRetirableGrants_retiringPrincipal = Lens.lens (\ListRetirableGrants' {retiringPrincipal} -> retiringPrincipal) (\s@ListRetirableGrants' {} a -> s {retiringPrincipal = a} :: ListRetirableGrants)

instance Core.AWSPager ListRetirableGrants where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGrantsResponse_truncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listGrantsResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRetirableGrants_marker
          Lens..~ rs
          Lens.^? listGrantsResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListRetirableGrants where
  type
    AWSResponse ListRetirableGrants =
      ListGrantsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable ListRetirableGrants where
  hashWithSalt _salt ListRetirableGrants' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` retiringPrincipal

instance Prelude.NFData ListRetirableGrants where
  rnf ListRetirableGrants' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf retiringPrincipal

instance Core.ToHeaders ListRetirableGrants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.ListRetirableGrants" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRetirableGrants where
  toJSON ListRetirableGrants' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("RetiringPrincipal" Core..= retiringPrincipal)
          ]
      )

instance Core.ToPath ListRetirableGrants where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRetirableGrants where
  toQuery = Prelude.const Prelude.mempty

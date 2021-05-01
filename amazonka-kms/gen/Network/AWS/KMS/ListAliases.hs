{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of aliases in the caller\'s AWS account and region. For more
-- information about aliases, see CreateAlias.
--
-- By default, the @ListAliases@ operation returns all aliases in the
-- account and region. To get only the aliases associated with a particular
-- customer master key (CMK), use the @KeyId@ parameter.
--
-- The @ListAliases@ response can include aliases that you created and
-- associated with your customer managed CMKs, and aliases that AWS created
-- and associated with AWS managed CMKs in your account. You can recognize
-- AWS aliases because their names have the format @aws\/\<service-name>@,
-- such as @aws\/dynamodb@.
--
-- The response might also include aliases that have no @TargetKeyId@
-- field. These are predefined aliases that AWS has created but has not yet
-- associated with a CMK. Aliases that AWS creates in your account,
-- including predefined aliases, do not count against your
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#aliases-limit AWS KMS aliases quota>.
--
-- __Cross-account use__: No. @ListAliases@ does not return aliases in
-- other AWS accounts.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListAliases>
-- (IAM policy)
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   DeleteAlias
--
-- -   UpdateAlias
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_limit,
    listAliases_keyId,
    listAliases_marker,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_nextMarker,
    listAliasesResponse_aliases,
    listAliasesResponse_truncated,
    listAliasesResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, AWS KMS does not return more than the
    -- specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 100, inclusive. If you do not include a value, it defaults to 50.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Lists only aliases that are associated with the specified CMK. Enter a
    -- CMK in your AWS account.
    --
    -- This parameter is optional. If you omit it, @ListAliases@ returns all
    -- aliases in the account and Region.
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
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listAliases_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
--
-- 'keyId', 'listAliases_keyId' - Lists only aliases that are associated with the specified CMK. Enter a
-- CMK in your AWS account.
--
-- This parameter is optional. If you omit it, @ListAliases@ returns all
-- aliases in the account and Region.
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
-- 'marker', 'listAliases_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
newListAliases ::
  ListAliases
newListAliases =
  ListAliases'
    { limit = Prelude.Nothing,
      keyId = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 100, inclusive. If you do not include a value, it defaults to 50.
listAliases_limit :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_limit = Lens.lens (\ListAliases' {limit} -> limit) (\s@ListAliases' {} a -> s {limit = a} :: ListAliases)

-- | Lists only aliases that are associated with the specified CMK. Enter a
-- CMK in your AWS account.
--
-- This parameter is optional. If you omit it, @ListAliases@ returns all
-- aliases in the account and Region.
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
listAliases_keyId :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_keyId = Lens.lens (\ListAliases' {keyId} -> keyId) (\s@ListAliases' {} a -> s {keyId = a} :: ListAliases)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listAliases_marker :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_marker = Lens.lens (\ListAliases' {marker} -> marker) (\s@ListAliases' {} a -> s {marker = a} :: ListAliases)

instance Pager.AWSPager ListAliases where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAliasesResponse_truncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listAliasesResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAliases_marker
          Lens..~ rs
          Lens.^? listAliasesResponse_nextMarker Prelude.. Lens._Just

instance Prelude.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Prelude..?> "NextMarker")
            Prelude.<*> (x Prelude..?> "Aliases" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Truncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases

instance Prelude.NFData ListAliases

instance Prelude.ToHeaders ListAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.ListAliases" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Limit" Prelude..=) Prelude.<$> limit,
            ("KeyId" Prelude..=) Prelude.<$> keyId,
            ("Marker" Prelude..=) Prelude.<$> marker
          ]
      )

instance Prelude.ToPath ListAliases where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of aliases.
    aliases :: Prelude.Maybe [AliasListEntry],
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listAliasesResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'aliases', 'listAliasesResponse_aliases' - A list of aliases.
--
-- 'truncated', 'listAliasesResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextMarker = Prelude.Nothing,
      aliases = Prelude.Nothing,
      truncated = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listAliasesResponse_nextMarker :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextMarker = Lens.lens (\ListAliasesResponse' {nextMarker} -> nextMarker) (\s@ListAliasesResponse' {} a -> s {nextMarker = a} :: ListAliasesResponse)

-- | A list of aliases.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [AliasListEntry])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listAliasesResponse_truncated :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Bool)
listAliasesResponse_truncated = Lens.lens (\ListAliasesResponse' {truncated} -> truncated) (\s@ListAliasesResponse' {} a -> s {truncated = a} :: ListAliasesResponse)

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse

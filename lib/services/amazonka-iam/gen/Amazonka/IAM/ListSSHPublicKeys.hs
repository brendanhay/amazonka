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
-- Module      : Amazonka.IAM.ListSSHPublicKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the SSH public keys associated with the
-- specified IAM user. If none exists, the operation returns an empty list.
--
-- The SSH public keys returned by this operation are used only for
-- authenticating the IAM user to an CodeCommit repository. For more
-- information about using SSH keys to authenticate to an CodeCommit
-- repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up CodeCommit for SSH connections>
-- in the /CodeCommit User Guide/.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListSSHPublicKeys
  ( -- * Creating a Request
    ListSSHPublicKeys (..),
    newListSSHPublicKeys,

    -- * Request Lenses
    listSSHPublicKeys_marker,
    listSSHPublicKeys_userName,
    listSSHPublicKeys_maxItems,

    -- * Destructuring the Response
    ListSSHPublicKeysResponse (..),
    newListSSHPublicKeysResponse,

    -- * Response Lenses
    listSSHPublicKeysResponse_marker,
    listSSHPublicKeysResponse_isTruncated,
    listSSHPublicKeysResponse_sSHPublicKeys,
    listSSHPublicKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSSHPublicKeys' smart constructor.
data ListSSHPublicKeys = ListSSHPublicKeys'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM user to list SSH public keys for. If none is
    -- specified, the @UserName@ field is determined implicitly based on the
    -- Amazon Web Services access key used to sign the request.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSSHPublicKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSSHPublicKeys_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'userName', 'listSSHPublicKeys_userName' - The name of the IAM user to list SSH public keys for. If none is
-- specified, the @UserName@ field is determined implicitly based on the
-- Amazon Web Services access key used to sign the request.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'maxItems', 'listSSHPublicKeys_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
newListSSHPublicKeys ::
  ListSSHPublicKeys
newListSSHPublicKeys =
  ListSSHPublicKeys'
    { marker = Prelude.Nothing,
      userName = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listSSHPublicKeys_marker :: Lens.Lens' ListSSHPublicKeys (Prelude.Maybe Prelude.Text)
listSSHPublicKeys_marker = Lens.lens (\ListSSHPublicKeys' {marker} -> marker) (\s@ListSSHPublicKeys' {} a -> s {marker = a} :: ListSSHPublicKeys)

-- | The name of the IAM user to list SSH public keys for. If none is
-- specified, the @UserName@ field is determined implicitly based on the
-- Amazon Web Services access key used to sign the request.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listSSHPublicKeys_userName :: Lens.Lens' ListSSHPublicKeys (Prelude.Maybe Prelude.Text)
listSSHPublicKeys_userName = Lens.lens (\ListSSHPublicKeys' {userName} -> userName) (\s@ListSSHPublicKeys' {} a -> s {userName = a} :: ListSSHPublicKeys)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listSSHPublicKeys_maxItems :: Lens.Lens' ListSSHPublicKeys (Prelude.Maybe Prelude.Natural)
listSSHPublicKeys_maxItems = Lens.lens (\ListSSHPublicKeys' {maxItems} -> maxItems) (\s@ListSSHPublicKeys' {} a -> s {maxItems = a} :: ListSSHPublicKeys)

instance Core.AWSPager ListSSHPublicKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSSHPublicKeysResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listSSHPublicKeysResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSSHPublicKeys_marker
          Lens..~ rs
          Lens.^? listSSHPublicKeysResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListSSHPublicKeys where
  type
    AWSResponse ListSSHPublicKeys =
      ListSSHPublicKeysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListSSHPublicKeysResult"
      ( \s h x ->
          ListSSHPublicKeysResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> ( x Data..@? "SSHPublicKeys" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSSHPublicKeys where
  hashWithSalt _salt ListSSHPublicKeys' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListSSHPublicKeys where
  rnf ListSSHPublicKeys' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListSSHPublicKeys where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSSHPublicKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSSHPublicKeys where
  toQuery ListSSHPublicKeys' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListSSHPublicKeys" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "UserName" Data.=: userName,
        "MaxItems" Data.=: maxItems
      ]

-- | Contains the response to a successful ListSSHPublicKeys request.
--
-- /See:/ 'newListSSHPublicKeysResponse' smart constructor.
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
  { -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | A list of the SSH public keys assigned to IAM user.
    sSHPublicKeys :: Prelude.Maybe [SSHPublicKeyMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSSHPublicKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSSHPublicKeysResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'listSSHPublicKeysResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'sSHPublicKeys', 'listSSHPublicKeysResponse_sSHPublicKeys' - A list of the SSH public keys assigned to IAM user.
--
-- 'httpStatus', 'listSSHPublicKeysResponse_httpStatus' - The response's http status code.
newListSSHPublicKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSSHPublicKeysResponse
newListSSHPublicKeysResponse pHttpStatus_ =
  ListSSHPublicKeysResponse'
    { marker =
        Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      sSHPublicKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listSSHPublicKeysResponse_marker :: Lens.Lens' ListSSHPublicKeysResponse (Prelude.Maybe Prelude.Text)
listSSHPublicKeysResponse_marker = Lens.lens (\ListSSHPublicKeysResponse' {marker} -> marker) (\s@ListSSHPublicKeysResponse' {} a -> s {marker = a} :: ListSSHPublicKeysResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listSSHPublicKeysResponse_isTruncated :: Lens.Lens' ListSSHPublicKeysResponse (Prelude.Maybe Prelude.Bool)
listSSHPublicKeysResponse_isTruncated = Lens.lens (\ListSSHPublicKeysResponse' {isTruncated} -> isTruncated) (\s@ListSSHPublicKeysResponse' {} a -> s {isTruncated = a} :: ListSSHPublicKeysResponse)

-- | A list of the SSH public keys assigned to IAM user.
listSSHPublicKeysResponse_sSHPublicKeys :: Lens.Lens' ListSSHPublicKeysResponse (Prelude.Maybe [SSHPublicKeyMetadata])
listSSHPublicKeysResponse_sSHPublicKeys = Lens.lens (\ListSSHPublicKeysResponse' {sSHPublicKeys} -> sSHPublicKeys) (\s@ListSSHPublicKeysResponse' {} a -> s {sSHPublicKeys = a} :: ListSSHPublicKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSSHPublicKeysResponse_httpStatus :: Lens.Lens' ListSSHPublicKeysResponse Prelude.Int
listSSHPublicKeysResponse_httpStatus = Lens.lens (\ListSSHPublicKeysResponse' {httpStatus} -> httpStatus) (\s@ListSSHPublicKeysResponse' {} a -> s {httpStatus = a} :: ListSSHPublicKeysResponse)

instance Prelude.NFData ListSSHPublicKeysResponse where
  rnf ListSSHPublicKeysResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf sSHPublicKeys
      `Prelude.seq` Prelude.rnf httpStatus

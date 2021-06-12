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
-- Module      : Network.AWS.IAM.ListSSHPublicKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the SSH public keys associated with the
-- specified IAM user. If none exists, the operation returns an empty list.
--
-- The SSH public keys returned by this operation are used only for
-- authenticating the IAM user to an AWS CodeCommit repository. For more
-- information about using SSH keys to authenticate to an AWS CodeCommit
-- repository, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH connections>
-- in the /AWS CodeCommit User Guide/.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSSHPublicKeys
  ( -- * Creating a Request
    ListSSHPublicKeys (..),
    newListSSHPublicKeys,

    -- * Request Lenses
    listSSHPublicKeys_userName,
    listSSHPublicKeys_maxItems,
    listSSHPublicKeys_marker,

    -- * Destructuring the Response
    ListSSHPublicKeysResponse (..),
    newListSSHPublicKeysResponse,

    -- * Response Lenses
    listSSHPublicKeysResponse_isTruncated,
    listSSHPublicKeysResponse_sSHPublicKeys,
    listSSHPublicKeysResponse_marker,
    listSSHPublicKeysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSSHPublicKeys' smart constructor.
data ListSSHPublicKeys = ListSSHPublicKeys'
  { -- | The name of the IAM user to list SSH public keys for. If none is
    -- specified, the @UserName@ field is determined implicitly based on the
    -- AWS access key used to sign the request.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Core.Maybe Core.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSSHPublicKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'listSSHPublicKeys_userName' - The name of the IAM user to list SSH public keys for. If none is
-- specified, the @UserName@ field is determined implicitly based on the
-- AWS access key used to sign the request.
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
--
-- 'marker', 'listSSHPublicKeys_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListSSHPublicKeys ::
  ListSSHPublicKeys
newListSSHPublicKeys =
  ListSSHPublicKeys'
    { userName = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The name of the IAM user to list SSH public keys for. If none is
-- specified, the @UserName@ field is determined implicitly based on the
-- AWS access key used to sign the request.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listSSHPublicKeys_userName :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Core.Text)
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
listSSHPublicKeys_maxItems :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Core.Natural)
listSSHPublicKeys_maxItems = Lens.lens (\ListSSHPublicKeys' {maxItems} -> maxItems) (\s@ListSSHPublicKeys' {} a -> s {maxItems = a} :: ListSSHPublicKeys)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listSSHPublicKeys_marker :: Lens.Lens' ListSSHPublicKeys (Core.Maybe Core.Text)
listSSHPublicKeys_marker = Lens.lens (\ListSSHPublicKeys' {marker} -> marker) (\s@ListSSHPublicKeys' {} a -> s {marker = a} :: ListSSHPublicKeys)

instance Core.AWSPager ListSSHPublicKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSSHPublicKeysResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listSSHPublicKeysResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSSHPublicKeys_marker
          Lens..~ rs
          Lens.^? listSSHPublicKeysResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListSSHPublicKeys where
  type
    AWSResponse ListSSHPublicKeys =
      ListSSHPublicKeysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSSHPublicKeysResult"
      ( \s h x ->
          ListSSHPublicKeysResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> ( x Core..@? "SSHPublicKeys" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSSHPublicKeys

instance Core.NFData ListSSHPublicKeys

instance Core.ToHeaders ListSSHPublicKeys where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSSHPublicKeys where
  toPath = Core.const "/"

instance Core.ToQuery ListSSHPublicKeys where
  toQuery ListSSHPublicKeys' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListSSHPublicKeys" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "UserName" Core.=: userName,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListSSHPublicKeys request.
--
-- /See:/ 'newListSSHPublicKeysResponse' smart constructor.
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | A list of the SSH public keys assigned to IAM user.
    sSHPublicKeys :: Core.Maybe [SSHPublicKeyMetadata],
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSSHPublicKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'marker', 'listSSHPublicKeysResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listSSHPublicKeysResponse_httpStatus' - The response's http status code.
newListSSHPublicKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSSHPublicKeysResponse
newListSSHPublicKeysResponse pHttpStatus_ =
  ListSSHPublicKeysResponse'
    { isTruncated =
        Core.Nothing,
      sSHPublicKeys = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listSSHPublicKeysResponse_isTruncated :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe Core.Bool)
listSSHPublicKeysResponse_isTruncated = Lens.lens (\ListSSHPublicKeysResponse' {isTruncated} -> isTruncated) (\s@ListSSHPublicKeysResponse' {} a -> s {isTruncated = a} :: ListSSHPublicKeysResponse)

-- | A list of the SSH public keys assigned to IAM user.
listSSHPublicKeysResponse_sSHPublicKeys :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe [SSHPublicKeyMetadata])
listSSHPublicKeysResponse_sSHPublicKeys = Lens.lens (\ListSSHPublicKeysResponse' {sSHPublicKeys} -> sSHPublicKeys) (\s@ListSSHPublicKeysResponse' {} a -> s {sSHPublicKeys = a} :: ListSSHPublicKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listSSHPublicKeysResponse_marker :: Lens.Lens' ListSSHPublicKeysResponse (Core.Maybe Core.Text)
listSSHPublicKeysResponse_marker = Lens.lens (\ListSSHPublicKeysResponse' {marker} -> marker) (\s@ListSSHPublicKeysResponse' {} a -> s {marker = a} :: ListSSHPublicKeysResponse)

-- | The response's http status code.
listSSHPublicKeysResponse_httpStatus :: Lens.Lens' ListSSHPublicKeysResponse Core.Int
listSSHPublicKeysResponse_httpStatus = Lens.lens (\ListSSHPublicKeysResponse' {httpStatus} -> httpStatus) (\s@ListSSHPublicKeysResponse' {} a -> s {httpStatus = a} :: ListSSHPublicKeysResponse)

instance Core.NFData ListSSHPublicKeysResponse

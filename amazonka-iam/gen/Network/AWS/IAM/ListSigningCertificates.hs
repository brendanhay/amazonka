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
-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the signing certificates associated with the
-- specified IAM user. If none exists, the operation returns an empty list.
--
-- Although each user is limited to a small number of signing certificates,
-- you can still paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- If the @UserName@ field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request for
-- this operation. This operation works for access keys under the AWS
-- account. Consequently, you can use this operation to manage AWS account
-- root user credentials even if the AWS account has no associated users.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSigningCertificates
  ( -- * Creating a Request
    ListSigningCertificates (..),
    newListSigningCertificates,

    -- * Request Lenses
    listSigningCertificates_userName,
    listSigningCertificates_maxItems,
    listSigningCertificates_marker,

    -- * Destructuring the Response
    ListSigningCertificatesResponse (..),
    newListSigningCertificatesResponse,

    -- * Response Lenses
    listSigningCertificatesResponse_isTruncated,
    listSigningCertificatesResponse_marker,
    listSigningCertificatesResponse_httpStatus,
    listSigningCertificatesResponse_certificates,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSigningCertificates' smart constructor.
data ListSigningCertificates = ListSigningCertificates'
  { -- | The name of the IAM user whose signing certificates you want to examine.
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
-- Create a value of 'ListSigningCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'listSigningCertificates_userName' - The name of the IAM user whose signing certificates you want to examine.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'maxItems', 'listSigningCertificates_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listSigningCertificates_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListSigningCertificates ::
  ListSigningCertificates
newListSigningCertificates =
  ListSigningCertificates'
    { userName = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The name of the IAM user whose signing certificates you want to examine.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listSigningCertificates_userName :: Lens.Lens' ListSigningCertificates (Core.Maybe Core.Text)
listSigningCertificates_userName = Lens.lens (\ListSigningCertificates' {userName} -> userName) (\s@ListSigningCertificates' {} a -> s {userName = a} :: ListSigningCertificates)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listSigningCertificates_maxItems :: Lens.Lens' ListSigningCertificates (Core.Maybe Core.Natural)
listSigningCertificates_maxItems = Lens.lens (\ListSigningCertificates' {maxItems} -> maxItems) (\s@ListSigningCertificates' {} a -> s {maxItems = a} :: ListSigningCertificates)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listSigningCertificates_marker :: Lens.Lens' ListSigningCertificates (Core.Maybe Core.Text)
listSigningCertificates_marker = Lens.lens (\ListSigningCertificates' {marker} -> marker) (\s@ListSigningCertificates' {} a -> s {marker = a} :: ListSigningCertificates)

instance Core.AWSPager ListSigningCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSigningCertificatesResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listSigningCertificatesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSigningCertificates_marker
          Lens..~ rs
          Lens.^? listSigningCertificatesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListSigningCertificates where
  type
    AWSResponse ListSigningCertificates =
      ListSigningCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSigningCertificatesResult"
      ( \s h x ->
          ListSigningCertificatesResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Certificates" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListSigningCertificates

instance Core.NFData ListSigningCertificates

instance Core.ToHeaders ListSigningCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSigningCertificates where
  toPath = Core.const "/"

instance Core.ToQuery ListSigningCertificates where
  toQuery ListSigningCertificates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListSigningCertificates" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "UserName" Core.=: userName,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListSigningCertificates request.
--
-- /See:/ 'newListSigningCertificatesResponse' smart constructor.
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of the user\'s signing certificate information.
    certificates :: [SigningCertificate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSigningCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listSigningCertificatesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listSigningCertificatesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listSigningCertificatesResponse_httpStatus' - The response's http status code.
--
-- 'certificates', 'listSigningCertificatesResponse_certificates' - A list of the user\'s signing certificate information.
newListSigningCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSigningCertificatesResponse
newListSigningCertificatesResponse pHttpStatus_ =
  ListSigningCertificatesResponse'
    { isTruncated =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      certificates = Core.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listSigningCertificatesResponse_isTruncated :: Lens.Lens' ListSigningCertificatesResponse (Core.Maybe Core.Bool)
listSigningCertificatesResponse_isTruncated = Lens.lens (\ListSigningCertificatesResponse' {isTruncated} -> isTruncated) (\s@ListSigningCertificatesResponse' {} a -> s {isTruncated = a} :: ListSigningCertificatesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listSigningCertificatesResponse_marker :: Lens.Lens' ListSigningCertificatesResponse (Core.Maybe Core.Text)
listSigningCertificatesResponse_marker = Lens.lens (\ListSigningCertificatesResponse' {marker} -> marker) (\s@ListSigningCertificatesResponse' {} a -> s {marker = a} :: ListSigningCertificatesResponse)

-- | The response's http status code.
listSigningCertificatesResponse_httpStatus :: Lens.Lens' ListSigningCertificatesResponse Core.Int
listSigningCertificatesResponse_httpStatus = Lens.lens (\ListSigningCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListSigningCertificatesResponse' {} a -> s {httpStatus = a} :: ListSigningCertificatesResponse)

-- | A list of the user\'s signing certificate information.
listSigningCertificatesResponse_certificates :: Lens.Lens' ListSigningCertificatesResponse [SigningCertificate]
listSigningCertificatesResponse_certificates = Lens.lens (\ListSigningCertificatesResponse' {certificates} -> certificates) (\s@ListSigningCertificatesResponse' {} a -> s {certificates = a} :: ListSigningCertificatesResponse) Core.. Lens._Coerce

instance Core.NFData ListSigningCertificatesResponse

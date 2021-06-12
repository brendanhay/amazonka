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
-- Module      : Network.AWS.IAM.ListServerCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the server certificates stored in IAM that have the specified path
-- prefix. If none exist, the operation returns an empty list.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- For more information about working with server certificates, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/. This topic also includes a list of AWS services
-- that can use the server certificates that you manage with IAM.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for a servercertificate, see
-- GetServerCertificate.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListServerCertificates
  ( -- * Creating a Request
    ListServerCertificates (..),
    newListServerCertificates,

    -- * Request Lenses
    listServerCertificates_pathPrefix,
    listServerCertificates_maxItems,
    listServerCertificates_marker,

    -- * Destructuring the Response
    ListServerCertificatesResponse (..),
    newListServerCertificatesResponse,

    -- * Response Lenses
    listServerCertificatesResponse_isTruncated,
    listServerCertificatesResponse_marker,
    listServerCertificatesResponse_httpStatus,
    listServerCertificatesResponse_serverCertificateMetadataList,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListServerCertificates' smart constructor.
data ListServerCertificates = ListServerCertificates'
  { -- | The path prefix for filtering the results. For example:
    -- @\/company\/servercerts@ would get all server certificates for which the
    -- path starts with @\/company\/servercerts@.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/), listing all server certificates. This parameter allows
    -- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
    -- of characters consisting of either a forward slash (\/) by itself or a
    -- string that must begin and end with forward slashes. In addition, it can
    -- contain any ASCII character from the ! (@\\u0021@) through the DEL
    -- character (@\\u007F@), including most punctuation characters, digits,
    -- and upper and lowercased letters.
    pathPrefix :: Core.Maybe Core.Text,
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
-- Create a value of 'ListServerCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefix', 'listServerCertificates_pathPrefix' - The path prefix for filtering the results. For example:
-- @\/company\/servercerts@ would get all server certificates for which the
-- path starts with @\/company\/servercerts@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all server certificates. This parameter allows
-- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
-- of characters consisting of either a forward slash (\/) by itself or a
-- string that must begin and end with forward slashes. In addition, it can
-- contain any ASCII character from the ! (@\\u0021@) through the DEL
-- character (@\\u007F@), including most punctuation characters, digits,
-- and upper and lowercased letters.
--
-- 'maxItems', 'listServerCertificates_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listServerCertificates_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListServerCertificates ::
  ListServerCertificates
newListServerCertificates =
  ListServerCertificates'
    { pathPrefix = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The path prefix for filtering the results. For example:
-- @\/company\/servercerts@ would get all server certificates for which the
-- path starts with @\/company\/servercerts@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all server certificates. This parameter allows
-- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
-- of characters consisting of either a forward slash (\/) by itself or a
-- string that must begin and end with forward slashes. In addition, it can
-- contain any ASCII character from the ! (@\\u0021@) through the DEL
-- character (@\\u007F@), including most punctuation characters, digits,
-- and upper and lowercased letters.
listServerCertificates_pathPrefix :: Lens.Lens' ListServerCertificates (Core.Maybe Core.Text)
listServerCertificates_pathPrefix = Lens.lens (\ListServerCertificates' {pathPrefix} -> pathPrefix) (\s@ListServerCertificates' {} a -> s {pathPrefix = a} :: ListServerCertificates)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listServerCertificates_maxItems :: Lens.Lens' ListServerCertificates (Core.Maybe Core.Natural)
listServerCertificates_maxItems = Lens.lens (\ListServerCertificates' {maxItems} -> maxItems) (\s@ListServerCertificates' {} a -> s {maxItems = a} :: ListServerCertificates)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listServerCertificates_marker :: Lens.Lens' ListServerCertificates (Core.Maybe Core.Text)
listServerCertificates_marker = Lens.lens (\ListServerCertificates' {marker} -> marker) (\s@ListServerCertificates' {} a -> s {marker = a} :: ListServerCertificates)

instance Core.AWSPager ListServerCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServerCertificatesResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listServerCertificatesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listServerCertificates_marker
          Lens..~ rs
          Lens.^? listServerCertificatesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListServerCertificates where
  type
    AWSResponse ListServerCertificates =
      ListServerCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListServerCertificatesResult"
      ( \s h x ->
          ListServerCertificatesResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "ServerCertificateMetadataList"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListServerCertificates

instance Core.NFData ListServerCertificates

instance Core.ToHeaders ListServerCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListServerCertificates where
  toPath = Core.const "/"

instance Core.ToQuery ListServerCertificates where
  toQuery ListServerCertificates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListServerCertificates" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "PathPrefix" Core.=: pathPrefix,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListServerCertificates request.
--
-- /See:/ 'newListServerCertificatesResponse' smart constructor.
data ListServerCertificatesResponse = ListServerCertificatesResponse'
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
    -- | A list of server certificates.
    serverCertificateMetadataList :: [ServerCertificateMetadata]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListServerCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listServerCertificatesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listServerCertificatesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listServerCertificatesResponse_httpStatus' - The response's http status code.
--
-- 'serverCertificateMetadataList', 'listServerCertificatesResponse_serverCertificateMetadataList' - A list of server certificates.
newListServerCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListServerCertificatesResponse
newListServerCertificatesResponse pHttpStatus_ =
  ListServerCertificatesResponse'
    { isTruncated =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      serverCertificateMetadataList = Core.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listServerCertificatesResponse_isTruncated :: Lens.Lens' ListServerCertificatesResponse (Core.Maybe Core.Bool)
listServerCertificatesResponse_isTruncated = Lens.lens (\ListServerCertificatesResponse' {isTruncated} -> isTruncated) (\s@ListServerCertificatesResponse' {} a -> s {isTruncated = a} :: ListServerCertificatesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listServerCertificatesResponse_marker :: Lens.Lens' ListServerCertificatesResponse (Core.Maybe Core.Text)
listServerCertificatesResponse_marker = Lens.lens (\ListServerCertificatesResponse' {marker} -> marker) (\s@ListServerCertificatesResponse' {} a -> s {marker = a} :: ListServerCertificatesResponse)

-- | The response's http status code.
listServerCertificatesResponse_httpStatus :: Lens.Lens' ListServerCertificatesResponse Core.Int
listServerCertificatesResponse_httpStatus = Lens.lens (\ListServerCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListServerCertificatesResponse' {} a -> s {httpStatus = a} :: ListServerCertificatesResponse)

-- | A list of server certificates.
listServerCertificatesResponse_serverCertificateMetadataList :: Lens.Lens' ListServerCertificatesResponse [ServerCertificateMetadata]
listServerCertificatesResponse_serverCertificateMetadataList = Lens.lens (\ListServerCertificatesResponse' {serverCertificateMetadataList} -> serverCertificateMetadataList) (\s@ListServerCertificatesResponse' {} a -> s {serverCertificateMetadataList = a} :: ListServerCertificatesResponse) Core.. Lens._Coerce

instance Core.NFData ListServerCertificatesResponse

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
-- Module      : Network.AWS.IAM.ListServerCertificateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified IAM server
-- certificate. The returned list of tags is sorted by tag key. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- For certificates in a Region supported by AWS Certificate Manager (ACM),
-- we recommend that you don\'t use IAM server certificates. Instead, use
-- ACM to provision, manage, and deploy your server certificates. For more
-- information about IAM server certificates,
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ListServerCertificateTags
  ( -- * Creating a Request
    ListServerCertificateTags (..),
    newListServerCertificateTags,

    -- * Request Lenses
    listServerCertificateTags_maxItems,
    listServerCertificateTags_marker,
    listServerCertificateTags_serverCertificateName,

    -- * Destructuring the Response
    ListServerCertificateTagsResponse (..),
    newListServerCertificateTagsResponse,

    -- * Response Lenses
    listServerCertificateTagsResponse_isTruncated,
    listServerCertificateTagsResponse_marker,
    listServerCertificateTagsResponse_httpStatus,
    listServerCertificateTagsResponse_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListServerCertificateTags' smart constructor.
data ListServerCertificateTags = ListServerCertificateTags'
  { -- | (Optional) Use this only when paginating results to indicate the maximum
    -- number of items that you want in the response. If additional items exist
    -- beyond the maximum that you specify, the @IsTruncated@ response element
    -- is @true@.
    --
    -- If you do not include this parameter, it defaults to 100. Note that IAM
    -- might return fewer results, even when more results are available. In
    -- that case, the @IsTruncated@ response element returns @true@, and
    -- @Marker@ contains a value to include in the subsequent call that tells
    -- the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM server certificate whose tags you want to see.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    serverCertificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServerCertificateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listServerCertificateTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
--
-- 'marker', 'listServerCertificateTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'serverCertificateName', 'listServerCertificateTags_serverCertificateName' - The name of the IAM server certificate whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListServerCertificateTags ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  ListServerCertificateTags
newListServerCertificateTags pServerCertificateName_ =
  ListServerCertificateTags'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      serverCertificateName = pServerCertificateName_
    }

-- | (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
listServerCertificateTags_maxItems :: Lens.Lens' ListServerCertificateTags (Prelude.Maybe Prelude.Natural)
listServerCertificateTags_maxItems = Lens.lens (\ListServerCertificateTags' {maxItems} -> maxItems) (\s@ListServerCertificateTags' {} a -> s {maxItems = a} :: ListServerCertificateTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listServerCertificateTags_marker :: Lens.Lens' ListServerCertificateTags (Prelude.Maybe Prelude.Text)
listServerCertificateTags_marker = Lens.lens (\ListServerCertificateTags' {marker} -> marker) (\s@ListServerCertificateTags' {} a -> s {marker = a} :: ListServerCertificateTags)

-- | The name of the IAM server certificate whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listServerCertificateTags_serverCertificateName :: Lens.Lens' ListServerCertificateTags Prelude.Text
listServerCertificateTags_serverCertificateName = Lens.lens (\ListServerCertificateTags' {serverCertificateName} -> serverCertificateName) (\s@ListServerCertificateTags' {} a -> s {serverCertificateName = a} :: ListServerCertificateTags)

instance Core.AWSRequest ListServerCertificateTags where
  type
    AWSResponse ListServerCertificateTags =
      ListServerCertificateTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListServerCertificateTagsResult"
      ( \s h x ->
          ListServerCertificateTagsResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListServerCertificateTags

instance Prelude.NFData ListServerCertificateTags

instance Core.ToHeaders ListServerCertificateTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListServerCertificateTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServerCertificateTags where
  toQuery ListServerCertificateTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListServerCertificateTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "ServerCertificateName"
          Core.=: serverCertificateName
      ]

-- | /See:/ 'newListServerCertificateTagsResponse' smart constructor.
data ListServerCertificateTagsResponse = ListServerCertificateTagsResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can use the @Marker@ request parameter to
    -- make a subsequent pagination request that retrieves more items. Note
    -- that IAM might return fewer than the @MaxItems@ number of results even
    -- when more results are available. Check @IsTruncated@ after every call to
    -- ensure that you receive all of your results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of tags that are currently attached to the IAM server
    -- certificate. Each tag consists of a key name and an associated value. If
    -- no tags are attached to the specified resource, the response contains an
    -- empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServerCertificateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listServerCertificateTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listServerCertificateTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listServerCertificateTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listServerCertificateTagsResponse_tags' - The list of tags that are currently attached to the IAM server
-- certificate. Each tag consists of a key name and an associated value. If
-- no tags are attached to the specified resource, the response contains an
-- empty list.
newListServerCertificateTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServerCertificateTagsResponse
newListServerCertificateTagsResponse pHttpStatus_ =
  ListServerCertificateTagsResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
listServerCertificateTagsResponse_isTruncated :: Lens.Lens' ListServerCertificateTagsResponse (Prelude.Maybe Prelude.Bool)
listServerCertificateTagsResponse_isTruncated = Lens.lens (\ListServerCertificateTagsResponse' {isTruncated} -> isTruncated) (\s@ListServerCertificateTagsResponse' {} a -> s {isTruncated = a} :: ListServerCertificateTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listServerCertificateTagsResponse_marker :: Lens.Lens' ListServerCertificateTagsResponse (Prelude.Maybe Prelude.Text)
listServerCertificateTagsResponse_marker = Lens.lens (\ListServerCertificateTagsResponse' {marker} -> marker) (\s@ListServerCertificateTagsResponse' {} a -> s {marker = a} :: ListServerCertificateTagsResponse)

-- | The response's http status code.
listServerCertificateTagsResponse_httpStatus :: Lens.Lens' ListServerCertificateTagsResponse Prelude.Int
listServerCertificateTagsResponse_httpStatus = Lens.lens (\ListServerCertificateTagsResponse' {httpStatus} -> httpStatus) (\s@ListServerCertificateTagsResponse' {} a -> s {httpStatus = a} :: ListServerCertificateTagsResponse)

-- | The list of tags that are currently attached to the IAM server
-- certificate. Each tag consists of a key name and an associated value. If
-- no tags are attached to the specified resource, the response contains an
-- empty list.
listServerCertificateTagsResponse_tags :: Lens.Lens' ListServerCertificateTagsResponse [Tag]
listServerCertificateTagsResponse_tags = Lens.lens (\ListServerCertificateTagsResponse' {tags} -> tags) (\s@ListServerCertificateTagsResponse' {} a -> s {tags = a} :: ListServerCertificateTagsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListServerCertificateTagsResponse

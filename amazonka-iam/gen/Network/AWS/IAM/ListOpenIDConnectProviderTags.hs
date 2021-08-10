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
-- Module      : Network.AWS.IAM.ListOpenIDConnectProviderTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified OpenID Connect
-- (OIDC)-compatible identity provider. The returned list of tags is sorted
-- by tag key. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc.html About web identity federation>.
--
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ListOpenIDConnectProviderTags
  ( -- * Creating a Request
    ListOpenIDConnectProviderTags (..),
    newListOpenIDConnectProviderTags,

    -- * Request Lenses
    listOpenIDConnectProviderTags_maxItems,
    listOpenIDConnectProviderTags_marker,
    listOpenIDConnectProviderTags_openIDConnectProviderArn,

    -- * Destructuring the Response
    ListOpenIDConnectProviderTagsResponse (..),
    newListOpenIDConnectProviderTagsResponse,

    -- * Response Lenses
    listOpenIDConnectProviderTagsResponse_isTruncated,
    listOpenIDConnectProviderTagsResponse_marker,
    listOpenIDConnectProviderTagsResponse_httpStatus,
    listOpenIDConnectProviderTagsResponse_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOpenIDConnectProviderTags' smart constructor.
data ListOpenIDConnectProviderTags = ListOpenIDConnectProviderTags'
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
    -- | The ARN of the OpenID Connect (OIDC) identity provider whose tags you
    -- want to see.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    openIDConnectProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProviderTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listOpenIDConnectProviderTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
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
-- 'marker', 'listOpenIDConnectProviderTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'openIDConnectProviderArn', 'listOpenIDConnectProviderTags_openIDConnectProviderArn' - The ARN of the OpenID Connect (OIDC) identity provider whose tags you
-- want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListOpenIDConnectProviderTags ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  ListOpenIDConnectProviderTags
newListOpenIDConnectProviderTags
  pOpenIDConnectProviderArn_ =
    ListOpenIDConnectProviderTags'
      { maxItems =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        openIDConnectProviderArn =
          pOpenIDConnectProviderArn_
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
listOpenIDConnectProviderTags_maxItems :: Lens.Lens' ListOpenIDConnectProviderTags (Prelude.Maybe Prelude.Natural)
listOpenIDConnectProviderTags_maxItems = Lens.lens (\ListOpenIDConnectProviderTags' {maxItems} -> maxItems) (\s@ListOpenIDConnectProviderTags' {} a -> s {maxItems = a} :: ListOpenIDConnectProviderTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listOpenIDConnectProviderTags_marker :: Lens.Lens' ListOpenIDConnectProviderTags (Prelude.Maybe Prelude.Text)
listOpenIDConnectProviderTags_marker = Lens.lens (\ListOpenIDConnectProviderTags' {marker} -> marker) (\s@ListOpenIDConnectProviderTags' {} a -> s {marker = a} :: ListOpenIDConnectProviderTags)

-- | The ARN of the OpenID Connect (OIDC) identity provider whose tags you
-- want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listOpenIDConnectProviderTags_openIDConnectProviderArn :: Lens.Lens' ListOpenIDConnectProviderTags Prelude.Text
listOpenIDConnectProviderTags_openIDConnectProviderArn = Lens.lens (\ListOpenIDConnectProviderTags' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@ListOpenIDConnectProviderTags' {} a -> s {openIDConnectProviderArn = a} :: ListOpenIDConnectProviderTags)

instance
  Core.AWSRequest
    ListOpenIDConnectProviderTags
  where
  type
    AWSResponse ListOpenIDConnectProviderTags =
      ListOpenIDConnectProviderTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListOpenIDConnectProviderTagsResult"
      ( \s h x ->
          ListOpenIDConnectProviderTagsResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance
  Prelude.Hashable
    ListOpenIDConnectProviderTags

instance Prelude.NFData ListOpenIDConnectProviderTags

instance Core.ToHeaders ListOpenIDConnectProviderTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListOpenIDConnectProviderTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOpenIDConnectProviderTags where
  toQuery ListOpenIDConnectProviderTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ListOpenIDConnectProviderTags" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "OpenIDConnectProviderArn"
          Core.=: openIDConnectProviderArn
      ]

-- | /See:/ 'newListOpenIDConnectProviderTagsResponse' smart constructor.
data ListOpenIDConnectProviderTagsResponse = ListOpenIDConnectProviderTagsResponse'
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
    -- | The list of tags that are currently attached to the OpenID Connect
    -- (OIDC) identity provider. Each tag consists of a key name and an
    -- associated value. If no tags are attached to the specified resource, the
    -- response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProviderTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listOpenIDConnectProviderTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listOpenIDConnectProviderTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listOpenIDConnectProviderTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listOpenIDConnectProviderTagsResponse_tags' - The list of tags that are currently attached to the OpenID Connect
-- (OIDC) identity provider. Each tag consists of a key name and an
-- associated value. If no tags are attached to the specified resource, the
-- response contains an empty list.
newListOpenIDConnectProviderTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOpenIDConnectProviderTagsResponse
newListOpenIDConnectProviderTagsResponse pHttpStatus_ =
  ListOpenIDConnectProviderTagsResponse'
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
listOpenIDConnectProviderTagsResponse_isTruncated :: Lens.Lens' ListOpenIDConnectProviderTagsResponse (Prelude.Maybe Prelude.Bool)
listOpenIDConnectProviderTagsResponse_isTruncated = Lens.lens (\ListOpenIDConnectProviderTagsResponse' {isTruncated} -> isTruncated) (\s@ListOpenIDConnectProviderTagsResponse' {} a -> s {isTruncated = a} :: ListOpenIDConnectProviderTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listOpenIDConnectProviderTagsResponse_marker :: Lens.Lens' ListOpenIDConnectProviderTagsResponse (Prelude.Maybe Prelude.Text)
listOpenIDConnectProviderTagsResponse_marker = Lens.lens (\ListOpenIDConnectProviderTagsResponse' {marker} -> marker) (\s@ListOpenIDConnectProviderTagsResponse' {} a -> s {marker = a} :: ListOpenIDConnectProviderTagsResponse)

-- | The response's http status code.
listOpenIDConnectProviderTagsResponse_httpStatus :: Lens.Lens' ListOpenIDConnectProviderTagsResponse Prelude.Int
listOpenIDConnectProviderTagsResponse_httpStatus = Lens.lens (\ListOpenIDConnectProviderTagsResponse' {httpStatus} -> httpStatus) (\s@ListOpenIDConnectProviderTagsResponse' {} a -> s {httpStatus = a} :: ListOpenIDConnectProviderTagsResponse)

-- | The list of tags that are currently attached to the OpenID Connect
-- (OIDC) identity provider. Each tag consists of a key name and an
-- associated value. If no tags are attached to the specified resource, the
-- response contains an empty list.
listOpenIDConnectProviderTagsResponse_tags :: Lens.Lens' ListOpenIDConnectProviderTagsResponse [Tag]
listOpenIDConnectProviderTagsResponse_tags = Lens.lens (\ListOpenIDConnectProviderTagsResponse' {tags} -> tags) (\s@ListOpenIDConnectProviderTagsResponse' {} a -> s {tags = a} :: ListOpenIDConnectProviderTagsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListOpenIDConnectProviderTagsResponse

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
-- Module      : Network.AWS.IAM.ListSAMLProviderTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified Security Assertion
-- Markup Language (SAML) identity provider. The returned list of tags is
-- sorted by tag key. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>.
--
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ListSAMLProviderTags
  ( -- * Creating a Request
    ListSAMLProviderTags (..),
    newListSAMLProviderTags,

    -- * Request Lenses
    listSAMLProviderTags_maxItems,
    listSAMLProviderTags_marker,
    listSAMLProviderTags_sAMLProviderArn,

    -- * Destructuring the Response
    ListSAMLProviderTagsResponse (..),
    newListSAMLProviderTagsResponse,

    -- * Response Lenses
    listSAMLProviderTagsResponse_isTruncated,
    listSAMLProviderTagsResponse_marker,
    listSAMLProviderTagsResponse_httpStatus,
    listSAMLProviderTagsResponse_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSAMLProviderTags' smart constructor.
data ListSAMLProviderTags = ListSAMLProviderTags'
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
    -- | The ARN of the Security Assertion Markup Language (SAML) identity
    -- provider whose tags you want to see.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    sAMLProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSAMLProviderTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listSAMLProviderTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
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
-- 'marker', 'listSAMLProviderTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'sAMLProviderArn', 'listSAMLProviderTags_sAMLProviderArn' - The ARN of the Security Assertion Markup Language (SAML) identity
-- provider whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListSAMLProviderTags ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
  ListSAMLProviderTags
newListSAMLProviderTags pSAMLProviderArn_ =
  ListSAMLProviderTags'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      sAMLProviderArn = pSAMLProviderArn_
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
listSAMLProviderTags_maxItems :: Lens.Lens' ListSAMLProviderTags (Prelude.Maybe Prelude.Natural)
listSAMLProviderTags_maxItems = Lens.lens (\ListSAMLProviderTags' {maxItems} -> maxItems) (\s@ListSAMLProviderTags' {} a -> s {maxItems = a} :: ListSAMLProviderTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listSAMLProviderTags_marker :: Lens.Lens' ListSAMLProviderTags (Prelude.Maybe Prelude.Text)
listSAMLProviderTags_marker = Lens.lens (\ListSAMLProviderTags' {marker} -> marker) (\s@ListSAMLProviderTags' {} a -> s {marker = a} :: ListSAMLProviderTags)

-- | The ARN of the Security Assertion Markup Language (SAML) identity
-- provider whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listSAMLProviderTags_sAMLProviderArn :: Lens.Lens' ListSAMLProviderTags Prelude.Text
listSAMLProviderTags_sAMLProviderArn = Lens.lens (\ListSAMLProviderTags' {sAMLProviderArn} -> sAMLProviderArn) (\s@ListSAMLProviderTags' {} a -> s {sAMLProviderArn = a} :: ListSAMLProviderTags)

instance Core.AWSRequest ListSAMLProviderTags where
  type
    AWSResponse ListSAMLProviderTags =
      ListSAMLProviderTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSAMLProviderTagsResult"
      ( \s h x ->
          ListSAMLProviderTagsResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListSAMLProviderTags

instance Prelude.NFData ListSAMLProviderTags

instance Core.ToHeaders ListSAMLProviderTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListSAMLProviderTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSAMLProviderTags where
  toQuery ListSAMLProviderTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListSAMLProviderTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "SAMLProviderArn" Core.=: sAMLProviderArn
      ]

-- | /See:/ 'newListSAMLProviderTagsResponse' smart constructor.
data ListSAMLProviderTagsResponse = ListSAMLProviderTagsResponse'
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
    -- | The list of tags that are currently attached to the Security Assertion
    -- Markup Language (SAML) identity provider. Each tag consists of a key
    -- name and an associated value. If no tags are attached to the specified
    -- resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSAMLProviderTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listSAMLProviderTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listSAMLProviderTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listSAMLProviderTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listSAMLProviderTagsResponse_tags' - The list of tags that are currently attached to the Security Assertion
-- Markup Language (SAML) identity provider. Each tag consists of a key
-- name and an associated value. If no tags are attached to the specified
-- resource, the response contains an empty list.
newListSAMLProviderTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSAMLProviderTagsResponse
newListSAMLProviderTagsResponse pHttpStatus_ =
  ListSAMLProviderTagsResponse'
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
listSAMLProviderTagsResponse_isTruncated :: Lens.Lens' ListSAMLProviderTagsResponse (Prelude.Maybe Prelude.Bool)
listSAMLProviderTagsResponse_isTruncated = Lens.lens (\ListSAMLProviderTagsResponse' {isTruncated} -> isTruncated) (\s@ListSAMLProviderTagsResponse' {} a -> s {isTruncated = a} :: ListSAMLProviderTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listSAMLProviderTagsResponse_marker :: Lens.Lens' ListSAMLProviderTagsResponse (Prelude.Maybe Prelude.Text)
listSAMLProviderTagsResponse_marker = Lens.lens (\ListSAMLProviderTagsResponse' {marker} -> marker) (\s@ListSAMLProviderTagsResponse' {} a -> s {marker = a} :: ListSAMLProviderTagsResponse)

-- | The response's http status code.
listSAMLProviderTagsResponse_httpStatus :: Lens.Lens' ListSAMLProviderTagsResponse Prelude.Int
listSAMLProviderTagsResponse_httpStatus = Lens.lens (\ListSAMLProviderTagsResponse' {httpStatus} -> httpStatus) (\s@ListSAMLProviderTagsResponse' {} a -> s {httpStatus = a} :: ListSAMLProviderTagsResponse)

-- | The list of tags that are currently attached to the Security Assertion
-- Markup Language (SAML) identity provider. Each tag consists of a key
-- name and an associated value. If no tags are attached to the specified
-- resource, the response contains an empty list.
listSAMLProviderTagsResponse_tags :: Lens.Lens' ListSAMLProviderTagsResponse [Tag]
listSAMLProviderTagsResponse_tags = Lens.lens (\ListSAMLProviderTagsResponse' {tags} -> tags) (\s@ListSAMLProviderTagsResponse' {} a -> s {tags = a} :: ListSAMLProviderTagsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListSAMLProviderTagsResponse

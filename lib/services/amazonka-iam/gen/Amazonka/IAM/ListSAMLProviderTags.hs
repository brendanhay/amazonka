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
-- Module      : Amazonka.IAM.ListSAMLProviderTags
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IAM.ListSAMLProviderTags
  ( -- * Creating a Request
    ListSAMLProviderTags (..),
    newListSAMLProviderTags,

    -- * Request Lenses
    listSAMLProviderTags_marker,
    listSAMLProviderTags_maxItems,
    listSAMLProviderTags_sAMLProviderArn,

    -- * Destructuring the Response
    ListSAMLProviderTagsResponse (..),
    newListSAMLProviderTagsResponse,

    -- * Response Lenses
    listSAMLProviderTagsResponse_marker,
    listSAMLProviderTagsResponse_isTruncated,
    listSAMLProviderTagsResponse_httpStatus,
    listSAMLProviderTagsResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSAMLProviderTags' smart constructor.
data ListSAMLProviderTags = ListSAMLProviderTags'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the Security Assertion Markup Language (SAML) identity
    -- provider whose tags you want to see.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
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
-- 'marker', 'listSAMLProviderTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listSAMLProviderTags_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'sAMLProviderArn', 'listSAMLProviderTags_sAMLProviderArn' - The ARN of the Security Assertion Markup Language (SAML) identity
-- provider whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListSAMLProviderTags ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
  ListSAMLProviderTags
newListSAMLProviderTags pSAMLProviderArn_ =
  ListSAMLProviderTags'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      sAMLProviderArn = pSAMLProviderArn_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listSAMLProviderTags_marker :: Lens.Lens' ListSAMLProviderTags (Prelude.Maybe Prelude.Text)
listSAMLProviderTags_marker = Lens.lens (\ListSAMLProviderTags' {marker} -> marker) (\s@ListSAMLProviderTags' {} a -> s {marker = a} :: ListSAMLProviderTags)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listSAMLProviderTags_maxItems :: Lens.Lens' ListSAMLProviderTags (Prelude.Maybe Prelude.Natural)
listSAMLProviderTags_maxItems = Lens.lens (\ListSAMLProviderTags' {maxItems} -> maxItems) (\s@ListSAMLProviderTags' {} a -> s {maxItems = a} :: ListSAMLProviderTags)

-- | The ARN of the Security Assertion Markup Language (SAML) identity
-- provider whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listSAMLProviderTags_sAMLProviderArn :: Lens.Lens' ListSAMLProviderTags Prelude.Text
listSAMLProviderTags_sAMLProviderArn = Lens.lens (\ListSAMLProviderTags' {sAMLProviderArn} -> sAMLProviderArn) (\s@ListSAMLProviderTags' {} a -> s {sAMLProviderArn = a} :: ListSAMLProviderTags)

instance Core.AWSRequest ListSAMLProviderTags where
  type
    AWSResponse ListSAMLProviderTags =
      ListSAMLProviderTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListSAMLProviderTagsResult"
      ( \s h x ->
          ListSAMLProviderTagsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListSAMLProviderTags where
  hashWithSalt _salt ListSAMLProviderTags' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` sAMLProviderArn

instance Prelude.NFData ListSAMLProviderTags where
  rnf ListSAMLProviderTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf sAMLProviderArn

instance Data.ToHeaders ListSAMLProviderTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSAMLProviderTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSAMLProviderTags where
  toQuery ListSAMLProviderTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListSAMLProviderTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "SAMLProviderArn" Data.=: sAMLProviderArn
      ]

-- | /See:/ 'newListSAMLProviderTagsResponse' smart constructor.
data ListSAMLProviderTagsResponse = ListSAMLProviderTagsResponse'
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
-- 'marker', 'listSAMLProviderTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'listSAMLProviderTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
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
    { marker =
        Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listSAMLProviderTagsResponse_marker :: Lens.Lens' ListSAMLProviderTagsResponse (Prelude.Maybe Prelude.Text)
listSAMLProviderTagsResponse_marker = Lens.lens (\ListSAMLProviderTagsResponse' {marker} -> marker) (\s@ListSAMLProviderTagsResponse' {} a -> s {marker = a} :: ListSAMLProviderTagsResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listSAMLProviderTagsResponse_isTruncated :: Lens.Lens' ListSAMLProviderTagsResponse (Prelude.Maybe Prelude.Bool)
listSAMLProviderTagsResponse_isTruncated = Lens.lens (\ListSAMLProviderTagsResponse' {isTruncated} -> isTruncated) (\s@ListSAMLProviderTagsResponse' {} a -> s {isTruncated = a} :: ListSAMLProviderTagsResponse)

-- | The response's http status code.
listSAMLProviderTagsResponse_httpStatus :: Lens.Lens' ListSAMLProviderTagsResponse Prelude.Int
listSAMLProviderTagsResponse_httpStatus = Lens.lens (\ListSAMLProviderTagsResponse' {httpStatus} -> httpStatus) (\s@ListSAMLProviderTagsResponse' {} a -> s {httpStatus = a} :: ListSAMLProviderTagsResponse)

-- | The list of tags that are currently attached to the Security Assertion
-- Markup Language (SAML) identity provider. Each tag consists of a key
-- name and an associated value. If no tags are attached to the specified
-- resource, the response contains an empty list.
listSAMLProviderTagsResponse_tags :: Lens.Lens' ListSAMLProviderTagsResponse [Tag]
listSAMLProviderTagsResponse_tags = Lens.lens (\ListSAMLProviderTagsResponse' {tags} -> tags) (\s@ListSAMLProviderTagsResponse' {} a -> s {tags = a} :: ListSAMLProviderTagsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSAMLProviderTagsResponse where
  rnf ListSAMLProviderTagsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags

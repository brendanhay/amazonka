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
-- Module      : Network.AWS.CertificateManagerPCA.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags, if any, that are associated with your private CA or one
-- that has been shared with you. Tags are labels that you can use to
-- identify and organize your CAs. Each tag consists of a key and an
-- optional value. Call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_TagCertificateAuthority.html TagCertificateAuthority>
-- action to add one or more tags to your CA. Call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority>
-- action to remove tags.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_maxResults,
    listTags_certificateAuthorityArn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Use this parameter when paginating results in a subsequent request after
    -- you receive a response with truncated results. Set it to the value of
    -- __NextToken__ from the response you just received.
    nextToken :: Core.Maybe Core.Text,
    -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response. If additional items exist beyond the
    -- number you specify, the __NextToken__ element is sent in the response.
    -- Use this __NextToken__ value in a subsequent request to retrieve
    -- additional items.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) that was returned when you called the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
    -- action. This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTags_nextToken' - Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- __NextToken__ from the response you just received.
--
-- 'maxResults', 'listTags_maxResults' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the __NextToken__ element is sent in the response.
-- Use this __NextToken__ value in a subsequent request to retrieve
-- additional items.
--
-- 'certificateAuthorityArn', 'listTags_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
newListTags ::
  -- | 'certificateAuthorityArn'
  Core.Text ->
  ListTags
newListTags pCertificateAuthorityArn_ =
  ListTags'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      certificateAuthorityArn = pCertificateAuthorityArn_
    }

-- | Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- __NextToken__ from the response you just received.
listTags_nextToken :: Lens.Lens' ListTags (Core.Maybe Core.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the __NextToken__ element is sent in the response.
-- Use this __NextToken__ value in a subsequent request to retrieve
-- additional items.
listTags_maxResults :: Lens.Lens' ListTags (Core.Maybe Core.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
listTags_certificateAuthorityArn :: Lens.Lens' ListTags Core.Text
listTags_certificateAuthorityArn = Lens.lens (\ListTags' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@ListTags' {} a -> s {certificateAuthorityArn = a} :: ListTags)

instance Core.AWSPager ListTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_tags Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTags_nextToken
          Lens..~ rs
          Lens.^? listTagsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTags

instance Core.NFData ListTags

instance Core.ToHeaders ListTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("ACMPrivateCA.ListTags" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTags where
  toJSON ListTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              )
          ]
      )

instance Core.ToPath ListTags where
  toPath = Core.const "/"

instance Core.ToQuery ListTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | When the list is truncated, this value is present and should be used for
    -- the __NextToken__ parameter in a subsequent pagination request.
    nextToken :: Core.Maybe Core.Text,
    -- | The tags associated with your private CA.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsResponse_nextToken' - When the list is truncated, this value is present and should be used for
-- the __NextToken__ parameter in a subsequent pagination request.
--
-- 'tags', 'listTagsResponse_tags' - The tags associated with your private CA.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the list is truncated, this value is present and should be used for
-- the __NextToken__ parameter in a subsequent pagination request.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Core.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | The tags associated with your private CA.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Core.Maybe (Core.NonEmpty Tag))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Core.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Core.NFData ListTagsResponse

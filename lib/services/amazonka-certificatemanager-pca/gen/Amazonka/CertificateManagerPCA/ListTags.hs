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
-- Module      : Amazonka.CertificateManagerPCA.ListTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags, if any, that are associated with your private CA or one
-- that has been shared with you. Tags are labels that you can use to
-- identify and organize your CAs. Each tag consists of a key and an
-- optional value. Call the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_TagCertificateAuthority.html TagCertificateAuthority>
-- action to add one or more tags to your CA. Call the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority>
-- action to remove tags.
--
-- This operation returns paginated results.
module Amazonka.CertificateManagerPCA.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_maxResults,
    listTags_nextToken,
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

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response. If additional items exist beyond the
    -- number you specify, the __NextToken__ element is sent in the response.
    -- Use this __NextToken__ value in a subsequent request to retrieve
    -- additional items.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter when paginating results in a subsequent request after
    -- you receive a response with truncated results. Set it to the value of
    -- __NextToken__ from the response you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that was returned when you called the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
    -- action. This must be of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTags_maxResults' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the __NextToken__ element is sent in the response.
-- Use this __NextToken__ value in a subsequent request to retrieve
-- additional items.
--
-- 'nextToken', 'listTags_nextToken' - Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- __NextToken__ from the response you just received.
--
-- 'certificateAuthorityArn', 'listTags_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
newListTags ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  ListTags
newListTags pCertificateAuthorityArn_ =
  ListTags'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      certificateAuthorityArn = pCertificateAuthorityArn_
    }

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the __NextToken__ element is sent in the response.
-- Use this __NextToken__ value in a subsequent request to retrieve
-- additional items.
listTags_maxResults :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- __NextToken__ from the response you just received.
listTags_nextToken :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
listTags_certificateAuthorityArn :: Lens.Lens' ListTags Prelude.Text
listTags_certificateAuthorityArn = Lens.lens (\ListTags' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@ListTags' {} a -> s {certificateAuthorityArn = a} :: ListTags)

instance Core.AWSPager ListTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_tags
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTags_nextToken
          Lens..~ rs
          Lens.^? listTagsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTags where
  hashWithSalt _salt ListTags' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` certificateAuthorityArn

instance Prelude.NFData ListTags where
  rnf ListTags' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf certificateAuthorityArn

instance Data.ToHeaders ListTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("ACMPrivateCA.ListTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTags where
  toJSON ListTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              )
          ]
      )

instance Data.ToPath ListTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | When the list is truncated, this value is present and should be used for
    -- the __NextToken__ parameter in a subsequent pagination request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with your private CA.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the list is truncated, this value is present and should be used for
-- the __NextToken__ parameter in a subsequent pagination request.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Prelude.Maybe Prelude.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | The tags associated with your private CA.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Prelude.NFData ListTagsResponse where
  rnf ListTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

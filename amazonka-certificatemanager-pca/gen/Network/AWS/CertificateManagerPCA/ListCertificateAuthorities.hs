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
-- Module      : Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the private certificate authorities that you created by using the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
  ( -- * Creating a Request
    ListCertificateAuthorities (..),
    newListCertificateAuthorities,

    -- * Request Lenses
    listCertificateAuthorities_nextToken,
    listCertificateAuthorities_maxResults,
    listCertificateAuthorities_resourceOwner,

    -- * Destructuring the Response
    ListCertificateAuthoritiesResponse (..),
    newListCertificateAuthoritiesResponse,

    -- * Response Lenses
    listCertificateAuthoritiesResponse_nextToken,
    listCertificateAuthoritiesResponse_certificateAuthorities,
    listCertificateAuthoritiesResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCertificateAuthorities' smart constructor.
data ListCertificateAuthorities = ListCertificateAuthorities'
  { -- | Use this parameter when paginating results in a subsequent request after
    -- you receive a response with truncated results. Set it to the value of
    -- the @NextToken@ parameter from the response you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response on each page. If additional items
    -- exist beyond the number you specify, the @NextToken@ element is sent in
    -- the response. Use this @NextToken@ value in a subsequent request to
    -- retrieve additional items.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter to filter the returned set of certificate authorities
    -- based on their owner. The default is SELF.
    resourceOwner :: Prelude.Maybe ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCertificateAuthorities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCertificateAuthorities_nextToken' - Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- the @NextToken@ parameter from the response you just received.
--
-- 'maxResults', 'listCertificateAuthorities_maxResults' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response on each page. If additional items
-- exist beyond the number you specify, the @NextToken@ element is sent in
-- the response. Use this @NextToken@ value in a subsequent request to
-- retrieve additional items.
--
-- 'resourceOwner', 'listCertificateAuthorities_resourceOwner' - Use this parameter to filter the returned set of certificate authorities
-- based on their owner. The default is SELF.
newListCertificateAuthorities ::
  ListCertificateAuthorities
newListCertificateAuthorities =
  ListCertificateAuthorities'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceOwner = Prelude.Nothing
    }

-- | Use this parameter when paginating results in a subsequent request after
-- you receive a response with truncated results. Set it to the value of
-- the @NextToken@ parameter from the response you just received.
listCertificateAuthorities_nextToken :: Lens.Lens' ListCertificateAuthorities (Prelude.Maybe Prelude.Text)
listCertificateAuthorities_nextToken = Lens.lens (\ListCertificateAuthorities' {nextToken} -> nextToken) (\s@ListCertificateAuthorities' {} a -> s {nextToken = a} :: ListCertificateAuthorities)

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response on each page. If additional items
-- exist beyond the number you specify, the @NextToken@ element is sent in
-- the response. Use this @NextToken@ value in a subsequent request to
-- retrieve additional items.
listCertificateAuthorities_maxResults :: Lens.Lens' ListCertificateAuthorities (Prelude.Maybe Prelude.Natural)
listCertificateAuthorities_maxResults = Lens.lens (\ListCertificateAuthorities' {maxResults} -> maxResults) (\s@ListCertificateAuthorities' {} a -> s {maxResults = a} :: ListCertificateAuthorities)

-- | Use this parameter to filter the returned set of certificate authorities
-- based on their owner. The default is SELF.
listCertificateAuthorities_resourceOwner :: Lens.Lens' ListCertificateAuthorities (Prelude.Maybe ResourceOwner)
listCertificateAuthorities_resourceOwner = Lens.lens (\ListCertificateAuthorities' {resourceOwner} -> resourceOwner) (\s@ListCertificateAuthorities' {} a -> s {resourceOwner = a} :: ListCertificateAuthorities)

instance Core.AWSPager ListCertificateAuthorities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCertificateAuthoritiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCertificateAuthoritiesResponse_certificateAuthorities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCertificateAuthorities_nextToken
          Lens..~ rs
          Lens.^? listCertificateAuthoritiesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCertificateAuthorities where
  type
    AWSResponse ListCertificateAuthorities =
      ListCertificateAuthoritiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificateAuthoritiesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CertificateAuthorities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificateAuthorities

instance Prelude.NFData ListCertificateAuthorities

instance Core.ToHeaders ListCertificateAuthorities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.ListCertificateAuthorities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCertificateAuthorities where
  toJSON ListCertificateAuthorities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ResourceOwner" Core..=) Prelude.<$> resourceOwner
          ]
      )

instance Core.ToPath ListCertificateAuthorities where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCertificateAuthorities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificateAuthoritiesResponse' smart constructor.
data ListCertificateAuthoritiesResponse = ListCertificateAuthoritiesResponse'
  { -- | When the list is truncated, this value is present and should be used for
    -- the @NextToken@ parameter in a subsequent pagination request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about each certificate authority you have created.
    certificateAuthorities :: Prelude.Maybe [CertificateAuthority],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCertificateAuthoritiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCertificateAuthoritiesResponse_nextToken' - When the list is truncated, this value is present and should be used for
-- the @NextToken@ parameter in a subsequent pagination request.
--
-- 'certificateAuthorities', 'listCertificateAuthoritiesResponse_certificateAuthorities' - Summary information about each certificate authority you have created.
--
-- 'httpStatus', 'listCertificateAuthoritiesResponse_httpStatus' - The response's http status code.
newListCertificateAuthoritiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificateAuthoritiesResponse
newListCertificateAuthoritiesResponse pHttpStatus_ =
  ListCertificateAuthoritiesResponse'
    { nextToken =
        Prelude.Nothing,
      certificateAuthorities =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the list is truncated, this value is present and should be used for
-- the @NextToken@ parameter in a subsequent pagination request.
listCertificateAuthoritiesResponse_nextToken :: Lens.Lens' ListCertificateAuthoritiesResponse (Prelude.Maybe Prelude.Text)
listCertificateAuthoritiesResponse_nextToken = Lens.lens (\ListCertificateAuthoritiesResponse' {nextToken} -> nextToken) (\s@ListCertificateAuthoritiesResponse' {} a -> s {nextToken = a} :: ListCertificateAuthoritiesResponse)

-- | Summary information about each certificate authority you have created.
listCertificateAuthoritiesResponse_certificateAuthorities :: Lens.Lens' ListCertificateAuthoritiesResponse (Prelude.Maybe [CertificateAuthority])
listCertificateAuthoritiesResponse_certificateAuthorities = Lens.lens (\ListCertificateAuthoritiesResponse' {certificateAuthorities} -> certificateAuthorities) (\s@ListCertificateAuthoritiesResponse' {} a -> s {certificateAuthorities = a} :: ListCertificateAuthoritiesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCertificateAuthoritiesResponse_httpStatus :: Lens.Lens' ListCertificateAuthoritiesResponse Prelude.Int
listCertificateAuthoritiesResponse_httpStatus = Lens.lens (\ListCertificateAuthoritiesResponse' {httpStatus} -> httpStatus) (\s@ListCertificateAuthoritiesResponse' {} a -> s {httpStatus = a} :: ListCertificateAuthoritiesResponse)

instance
  Prelude.NFData
    ListCertificateAuthoritiesResponse

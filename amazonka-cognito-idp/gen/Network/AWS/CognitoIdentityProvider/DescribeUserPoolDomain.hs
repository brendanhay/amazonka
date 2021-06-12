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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a domain.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
  ( -- * Creating a Request
    DescribeUserPoolDomain (..),
    newDescribeUserPoolDomain,

    -- * Request Lenses
    describeUserPoolDomain_domain,

    -- * Destructuring the Response
    DescribeUserPoolDomainResponse (..),
    newDescribeUserPoolDomainResponse,

    -- * Response Lenses
    describeUserPoolDomainResponse_domainDescription,
    describeUserPoolDomainResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserPoolDomain' smart constructor.
data DescribeUserPoolDomain = DescribeUserPoolDomain'
  { -- | The domain string.
    domain :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPoolDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'describeUserPoolDomain_domain' - The domain string.
newDescribeUserPoolDomain ::
  -- | 'domain'
  Core.Text ->
  DescribeUserPoolDomain
newDescribeUserPoolDomain pDomain_ =
  DescribeUserPoolDomain' {domain = pDomain_}

-- | The domain string.
describeUserPoolDomain_domain :: Lens.Lens' DescribeUserPoolDomain Core.Text
describeUserPoolDomain_domain = Lens.lens (\DescribeUserPoolDomain' {domain} -> domain) (\s@DescribeUserPoolDomain' {} a -> s {domain = a} :: DescribeUserPoolDomain)

instance Core.AWSRequest DescribeUserPoolDomain where
  type
    AWSResponse DescribeUserPoolDomain =
      DescribeUserPoolDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolDomainResponse'
            Core.<$> (x Core..?> "DomainDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserPoolDomain

instance Core.NFData DescribeUserPoolDomain

instance Core.ToHeaders DescribeUserPoolDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeUserPoolDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserPoolDomain where
  toJSON DescribeUserPoolDomain' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Domain" Core..= domain)]
      )

instance Core.ToPath DescribeUserPoolDomain where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserPoolDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserPoolDomainResponse' smart constructor.
data DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse'
  { -- | A domain description object containing information about the domain.
    domainDescription :: Core.Maybe DomainDescriptionType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPoolDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainDescription', 'describeUserPoolDomainResponse_domainDescription' - A domain description object containing information about the domain.
--
-- 'httpStatus', 'describeUserPoolDomainResponse_httpStatus' - The response's http status code.
newDescribeUserPoolDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserPoolDomainResponse
newDescribeUserPoolDomainResponse pHttpStatus_ =
  DescribeUserPoolDomainResponse'
    { domainDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A domain description object containing information about the domain.
describeUserPoolDomainResponse_domainDescription :: Lens.Lens' DescribeUserPoolDomainResponse (Core.Maybe DomainDescriptionType)
describeUserPoolDomainResponse_domainDescription = Lens.lens (\DescribeUserPoolDomainResponse' {domainDescription} -> domainDescription) (\s@DescribeUserPoolDomainResponse' {} a -> s {domainDescription = a} :: DescribeUserPoolDomainResponse)

-- | The response's http status code.
describeUserPoolDomainResponse_httpStatus :: Lens.Lens' DescribeUserPoolDomainResponse Core.Int
describeUserPoolDomainResponse_httpStatus = Lens.lens (\DescribeUserPoolDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeUserPoolDomainResponse' {} a -> s {httpStatus = a} :: DescribeUserPoolDomainResponse)

instance Core.NFData DescribeUserPoolDomainResponse

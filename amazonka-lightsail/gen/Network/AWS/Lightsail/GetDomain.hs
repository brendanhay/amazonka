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
-- Module      : Network.AWS.Lightsail.GetDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain recordset.
module Network.AWS.Lightsail.GetDomain
  ( -- * Creating a Request
    GetDomain (..),
    newGetDomain,

    -- * Request Lenses
    getDomain_domainName,

    -- * Destructuring the Response
    GetDomainResponse (..),
    newGetDomainResponse,

    -- * Response Lenses
    getDomainResponse_domain,
    getDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDomain' smart constructor.
data GetDomain = GetDomain'
  { -- | The domain name for which your want to return information about.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomain_domainName' - The domain name for which your want to return information about.
newGetDomain ::
  -- | 'domainName'
  Core.Text ->
  GetDomain
newGetDomain pDomainName_ =
  GetDomain' {domainName = pDomainName_}

-- | The domain name for which your want to return information about.
getDomain_domainName :: Lens.Lens' GetDomain Core.Text
getDomain_domainName = Lens.lens (\GetDomain' {domainName} -> domainName) (\s@GetDomain' {} a -> s {domainName = a} :: GetDomain)

instance Core.AWSRequest GetDomain where
  type AWSResponse GetDomain = GetDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Core.<$> (x Core..?> "domain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDomain

instance Core.NFData GetDomain

instance Core.ToHeaders GetDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.GetDomain" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDomain where
  toJSON GetDomain' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("domainName" Core..= domainName)]
      )

instance Core.ToPath GetDomain where
  toPath = Core.const "/"

instance Core.ToQuery GetDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | An array of key-value pairs containing information about your get domain
    -- request.
    domain :: Core.Maybe Domain,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'getDomainResponse_domain' - An array of key-value pairs containing information about your get domain
-- request.
--
-- 'httpStatus', 'getDomainResponse_httpStatus' - The response's http status code.
newGetDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDomainResponse
newGetDomainResponse pHttpStatus_ =
  GetDomainResponse'
    { domain = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about your get domain
-- request.
getDomainResponse_domain :: Lens.Lens' GetDomainResponse (Core.Maybe Domain)
getDomainResponse_domain = Lens.lens (\GetDomainResponse' {domain} -> domain) (\s@GetDomainResponse' {} a -> s {domain = a} :: GetDomainResponse)

-- | The response's http status code.
getDomainResponse_httpStatus :: Lens.Lens' GetDomainResponse Core.Int
getDomainResponse_httpStatus = Lens.lens (\GetDomainResponse' {httpStatus} -> httpStatus) (\s@GetDomainResponse' {} a -> s {httpStatus = a} :: GetDomainResponse)

instance Core.NFData GetDomainResponse

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
-- Module      : Amazonka.Lightsail.GetDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain recordset.
module Amazonka.Lightsail.GetDomain
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomain' smart constructor.
data GetDomain = GetDomain'
  { -- | The domain name for which your want to return information about.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetDomain
newGetDomain pDomainName_ =
  GetDomain' {domainName = pDomainName_}

-- | The domain name for which your want to return information about.
getDomain_domainName :: Lens.Lens' GetDomain Prelude.Text
getDomain_domainName = Lens.lens (\GetDomain' {domainName} -> domainName) (\s@GetDomain' {} a -> s {domainName = a} :: GetDomain)

instance Core.AWSRequest GetDomain where
  type AWSResponse GetDomain = GetDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Prelude.<$> (x Data..?> "domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomain where
  hashWithSalt _salt GetDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomain where
  rnf GetDomain' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDomain where
  toJSON GetDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("domainName" Data..= domainName)]
      )

instance Data.ToPath GetDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | An array of key-value pairs containing information about your get domain
    -- request.
    domain :: Prelude.Maybe Domain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDomainResponse
newGetDomainResponse pHttpStatus_ =
  GetDomainResponse'
    { domain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about your get domain
-- request.
getDomainResponse_domain :: Lens.Lens' GetDomainResponse (Prelude.Maybe Domain)
getDomainResponse_domain = Lens.lens (\GetDomainResponse' {domain} -> domain) (\s@GetDomainResponse' {} a -> s {domain = a} :: GetDomainResponse)

-- | The response's http status code.
getDomainResponse_httpStatus :: Lens.Lens' GetDomainResponse Prelude.Int
getDomainResponse_httpStatus = Lens.lens (\GetDomainResponse' {httpStatus} -> httpStatus) (\s@GetDomainResponse' {} a -> s {httpStatus = a} :: GetDomainResponse)

instance Prelude.NFData GetDomainResponse where
  rnf GetDomainResponse' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf httpStatus

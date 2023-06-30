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
-- Module      : Amazonka.ApiGatewayV2.GetDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a domain name.
module Amazonka.ApiGatewayV2.GetDomainName
  ( -- * Creating a Request
    GetDomainName (..),
    newGetDomainName,

    -- * Request Lenses
    getDomainName_domainName,

    -- * Destructuring the Response
    GetDomainNameResponse (..),
    newGetDomainNameResponse,

    -- * Response Lenses
    getDomainNameResponse_apiMappingSelectionExpression,
    getDomainNameResponse_domainName,
    getDomainNameResponse_domainNameConfigurations,
    getDomainNameResponse_mutualTlsAuthentication,
    getDomainNameResponse_tags,
    getDomainNameResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomainName' smart constructor.
data GetDomainName = GetDomainName'
  { -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomainName_domainName' - The domain name.
newGetDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  GetDomainName
newGetDomainName pDomainName_ =
  GetDomainName' {domainName = pDomainName_}

-- | The domain name.
getDomainName_domainName :: Lens.Lens' GetDomainName Prelude.Text
getDomainName_domainName = Lens.lens (\GetDomainName' {domainName} -> domainName) (\s@GetDomainName' {} a -> s {domainName = a} :: GetDomainName)

instance Core.AWSRequest GetDomainName where
  type
    AWSResponse GetDomainName =
      GetDomainNameResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNameResponse'
            Prelude.<$> (x Data..?> "apiMappingSelectionExpression")
            Prelude.<*> (x Data..?> "domainName")
            Prelude.<*> ( x
                            Data..?> "domainNameConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "mutualTlsAuthentication")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainName where
  hashWithSalt _salt GetDomainName' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomainName where
  rnf GetDomainName' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomainName where
  toPath GetDomainName' {..} =
    Prelude.mconcat
      ["/v2/domainnames/", Data.toBS domainName]

instance Data.ToQuery GetDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainNameResponse' smart constructor.
data GetDomainNameResponse = GetDomainNameResponse'
  { -- | The API mapping selection expression.
    apiMappingSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the DomainName resource.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The domain name configurations.
    domainNameConfigurations :: Prelude.Maybe [DomainNameConfiguration],
    -- | The mutual TLS authentication configuration for a custom domain name.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthentication,
    -- | The collection of tags associated with a domain name.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingSelectionExpression', 'getDomainNameResponse_apiMappingSelectionExpression' - The API mapping selection expression.
--
-- 'domainName', 'getDomainNameResponse_domainName' - The name of the DomainName resource.
--
-- 'domainNameConfigurations', 'getDomainNameResponse_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'getDomainNameResponse_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'tags', 'getDomainNameResponse_tags' - The collection of tags associated with a domain name.
--
-- 'httpStatus', 'getDomainNameResponse_httpStatus' - The response's http status code.
newGetDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainNameResponse
newGetDomainNameResponse pHttpStatus_ =
  GetDomainNameResponse'
    { apiMappingSelectionExpression =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainNameConfigurations = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API mapping selection expression.
getDomainNameResponse_apiMappingSelectionExpression :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe Prelude.Text)
getDomainNameResponse_apiMappingSelectionExpression = Lens.lens (\GetDomainNameResponse' {apiMappingSelectionExpression} -> apiMappingSelectionExpression) (\s@GetDomainNameResponse' {} a -> s {apiMappingSelectionExpression = a} :: GetDomainNameResponse)

-- | The name of the DomainName resource.
getDomainNameResponse_domainName :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe Prelude.Text)
getDomainNameResponse_domainName = Lens.lens (\GetDomainNameResponse' {domainName} -> domainName) (\s@GetDomainNameResponse' {} a -> s {domainName = a} :: GetDomainNameResponse)

-- | The domain name configurations.
getDomainNameResponse_domainNameConfigurations :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe [DomainNameConfiguration])
getDomainNameResponse_domainNameConfigurations = Lens.lens (\GetDomainNameResponse' {domainNameConfigurations} -> domainNameConfigurations) (\s@GetDomainNameResponse' {} a -> s {domainNameConfigurations = a} :: GetDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
getDomainNameResponse_mutualTlsAuthentication :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe MutualTlsAuthentication)
getDomainNameResponse_mutualTlsAuthentication = Lens.lens (\GetDomainNameResponse' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@GetDomainNameResponse' {} a -> s {mutualTlsAuthentication = a} :: GetDomainNameResponse)

-- | The collection of tags associated with a domain name.
getDomainNameResponse_tags :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDomainNameResponse_tags = Lens.lens (\GetDomainNameResponse' {tags} -> tags) (\s@GetDomainNameResponse' {} a -> s {tags = a} :: GetDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDomainNameResponse_httpStatus :: Lens.Lens' GetDomainNameResponse Prelude.Int
getDomainNameResponse_httpStatus = Lens.lens (\GetDomainNameResponse' {httpStatus} -> httpStatus) (\s@GetDomainNameResponse' {} a -> s {httpStatus = a} :: GetDomainNameResponse)

instance Prelude.NFData GetDomainNameResponse where
  rnf GetDomainNameResponse' {..} =
    Prelude.rnf apiMappingSelectionExpression
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

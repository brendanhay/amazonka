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
-- Module      : Amazonka.ApiGatewayV2.UpdateDomainName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain name.
module Amazonka.ApiGatewayV2.UpdateDomainName
  ( -- * Creating a Request
    UpdateDomainName (..),
    newUpdateDomainName,

    -- * Request Lenses
    updateDomainName_domainNameConfigurations,
    updateDomainName_mutualTlsAuthentication,
    updateDomainName_domainName,

    -- * Destructuring the Response
    UpdateDomainNameResponse (..),
    newUpdateDomainNameResponse,

    -- * Response Lenses
    updateDomainNameResponse_apiMappingSelectionExpression,
    updateDomainNameResponse_domainName,
    updateDomainNameResponse_domainNameConfigurations,
    updateDomainNameResponse_mutualTlsAuthentication,
    updateDomainNameResponse_tags,
    updateDomainNameResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a DomainName.
--
-- /See:/ 'newUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { -- | The domain name configurations.
    domainNameConfigurations :: Prelude.Maybe [DomainNameConfiguration],
    -- | The mutual TLS authentication configuration for a custom domain name.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthenticationInput,
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfigurations', 'updateDomainName_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'updateDomainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'domainName', 'updateDomainName_domainName' - The domain name.
newUpdateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainName
newUpdateDomainName pDomainName_ =
  UpdateDomainName'
    { domainNameConfigurations =
        Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The domain name configurations.
updateDomainName_domainNameConfigurations :: Lens.Lens' UpdateDomainName (Prelude.Maybe [DomainNameConfiguration])
updateDomainName_domainNameConfigurations = Lens.lens (\UpdateDomainName' {domainNameConfigurations} -> domainNameConfigurations) (\s@UpdateDomainName' {} a -> s {domainNameConfigurations = a} :: UpdateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
updateDomainName_mutualTlsAuthentication :: Lens.Lens' UpdateDomainName (Prelude.Maybe MutualTlsAuthenticationInput)
updateDomainName_mutualTlsAuthentication = Lens.lens (\UpdateDomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@UpdateDomainName' {} a -> s {mutualTlsAuthentication = a} :: UpdateDomainName)

-- | The domain name.
updateDomainName_domainName :: Lens.Lens' UpdateDomainName Prelude.Text
updateDomainName_domainName = Lens.lens (\UpdateDomainName' {domainName} -> domainName) (\s@UpdateDomainName' {} a -> s {domainName = a} :: UpdateDomainName)

instance Core.AWSRequest UpdateDomainName where
  type
    AWSResponse UpdateDomainName =
      UpdateDomainNameResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainNameResponse'
            Prelude.<$> (x Data..?> "apiMappingSelectionExpression")
            Prelude.<*> (x Data..?> "domainName")
            Prelude.<*> ( x Data..?> "domainNameConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "mutualTlsAuthentication")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainName where
  hashWithSalt _salt UpdateDomainName' {..} =
    _salt
      `Prelude.hashWithSalt` domainNameConfigurations
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainName where
  rnf UpdateDomainName' {..} =
    Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomainName where
  toJSON UpdateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domainNameConfigurations" Data..=)
              Prelude.<$> domainNameConfigurations,
            ("mutualTlsAuthentication" Data..=)
              Prelude.<$> mutualTlsAuthentication
          ]
      )

instance Data.ToPath UpdateDomainName where
  toPath UpdateDomainName' {..} =
    Prelude.mconcat
      ["/v2/domainnames/", Data.toBS domainName]

instance Data.ToQuery UpdateDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainNameResponse' smart constructor.
data UpdateDomainNameResponse = UpdateDomainNameResponse'
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
-- Create a value of 'UpdateDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingSelectionExpression', 'updateDomainNameResponse_apiMappingSelectionExpression' - The API mapping selection expression.
--
-- 'domainName', 'updateDomainNameResponse_domainName' - The name of the DomainName resource.
--
-- 'domainNameConfigurations', 'updateDomainNameResponse_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'updateDomainNameResponse_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'tags', 'updateDomainNameResponse_tags' - The collection of tags associated with a domain name.
--
-- 'httpStatus', 'updateDomainNameResponse_httpStatus' - The response's http status code.
newUpdateDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainNameResponse
newUpdateDomainNameResponse pHttpStatus_ =
  UpdateDomainNameResponse'
    { apiMappingSelectionExpression =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainNameConfigurations = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API mapping selection expression.
updateDomainNameResponse_apiMappingSelectionExpression :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe Prelude.Text)
updateDomainNameResponse_apiMappingSelectionExpression = Lens.lens (\UpdateDomainNameResponse' {apiMappingSelectionExpression} -> apiMappingSelectionExpression) (\s@UpdateDomainNameResponse' {} a -> s {apiMappingSelectionExpression = a} :: UpdateDomainNameResponse)

-- | The name of the DomainName resource.
updateDomainNameResponse_domainName :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe Prelude.Text)
updateDomainNameResponse_domainName = Lens.lens (\UpdateDomainNameResponse' {domainName} -> domainName) (\s@UpdateDomainNameResponse' {} a -> s {domainName = a} :: UpdateDomainNameResponse)

-- | The domain name configurations.
updateDomainNameResponse_domainNameConfigurations :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe [DomainNameConfiguration])
updateDomainNameResponse_domainNameConfigurations = Lens.lens (\UpdateDomainNameResponse' {domainNameConfigurations} -> domainNameConfigurations) (\s@UpdateDomainNameResponse' {} a -> s {domainNameConfigurations = a} :: UpdateDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
updateDomainNameResponse_mutualTlsAuthentication :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe MutualTlsAuthentication)
updateDomainNameResponse_mutualTlsAuthentication = Lens.lens (\UpdateDomainNameResponse' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@UpdateDomainNameResponse' {} a -> s {mutualTlsAuthentication = a} :: UpdateDomainNameResponse)

-- | The collection of tags associated with a domain name.
updateDomainNameResponse_tags :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDomainNameResponse_tags = Lens.lens (\UpdateDomainNameResponse' {tags} -> tags) (\s@UpdateDomainNameResponse' {} a -> s {tags = a} :: UpdateDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateDomainNameResponse_httpStatus :: Lens.Lens' UpdateDomainNameResponse Prelude.Int
updateDomainNameResponse_httpStatus = Lens.lens (\UpdateDomainNameResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainNameResponse' {} a -> s {httpStatus = a} :: UpdateDomainNameResponse)

instance Prelude.NFData UpdateDomainNameResponse where
  rnf UpdateDomainNameResponse' {..} =
    Prelude.rnf apiMappingSelectionExpression
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

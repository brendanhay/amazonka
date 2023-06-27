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
-- Module      : Amazonka.ApiGatewayV2.CreateDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain name.
module Amazonka.ApiGatewayV2.CreateDomainName
  ( -- * Creating a Request
    CreateDomainName (..),
    newCreateDomainName,

    -- * Request Lenses
    createDomainName_domainNameConfigurations,
    createDomainName_mutualTlsAuthentication,
    createDomainName_tags,
    createDomainName_domainName,

    -- * Destructuring the Response
    CreateDomainNameResponse (..),
    newCreateDomainNameResponse,

    -- * Response Lenses
    createDomainNameResponse_apiMappingSelectionExpression,
    createDomainNameResponse_domainName,
    createDomainNameResponse_domainNameConfigurations,
    createDomainNameResponse_mutualTlsAuthentication,
    createDomainNameResponse_tags,
    createDomainNameResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new DomainName resource to represent a domain name.
--
-- /See:/ 'newCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | The domain name configurations.
    domainNameConfigurations :: Prelude.Maybe [DomainNameConfiguration],
    -- | The mutual TLS authentication configuration for a custom domain name.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthenticationInput,
    -- | The collection of tags associated with a domain name.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfigurations', 'createDomainName_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'createDomainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'tags', 'createDomainName_tags' - The collection of tags associated with a domain name.
--
-- 'domainName', 'createDomainName_domainName' - The domain name.
newCreateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomainName
newCreateDomainName pDomainName_ =
  CreateDomainName'
    { domainNameConfigurations =
        Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The domain name configurations.
createDomainName_domainNameConfigurations :: Lens.Lens' CreateDomainName (Prelude.Maybe [DomainNameConfiguration])
createDomainName_domainNameConfigurations = Lens.lens (\CreateDomainName' {domainNameConfigurations} -> domainNameConfigurations) (\s@CreateDomainName' {} a -> s {domainNameConfigurations = a} :: CreateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
createDomainName_mutualTlsAuthentication :: Lens.Lens' CreateDomainName (Prelude.Maybe MutualTlsAuthenticationInput)
createDomainName_mutualTlsAuthentication = Lens.lens (\CreateDomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@CreateDomainName' {} a -> s {mutualTlsAuthentication = a} :: CreateDomainName)

-- | The collection of tags associated with a domain name.
createDomainName_tags :: Lens.Lens' CreateDomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainName_tags = Lens.lens (\CreateDomainName' {tags} -> tags) (\s@CreateDomainName' {} a -> s {tags = a} :: CreateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | The domain name.
createDomainName_domainName :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_domainName = Lens.lens (\CreateDomainName' {domainName} -> domainName) (\s@CreateDomainName' {} a -> s {domainName = a} :: CreateDomainName)

instance Core.AWSRequest CreateDomainName where
  type
    AWSResponse CreateDomainName =
      CreateDomainNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainNameResponse'
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

instance Prelude.Hashable CreateDomainName where
  hashWithSalt _salt CreateDomainName' {..} =
    _salt
      `Prelude.hashWithSalt` domainNameConfigurations
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateDomainName where
  rnf CreateDomainName' {..} =
    Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domainNameConfigurations" Data..=)
              Prelude.<$> domainNameConfigurations,
            ("mutualTlsAuthentication" Data..=)
              Prelude.<$> mutualTlsAuthentication,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("domainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateDomainName where
  toPath = Prelude.const "/v2/domainnames"

instance Data.ToQuery CreateDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainNameResponse' smart constructor.
data CreateDomainNameResponse = CreateDomainNameResponse'
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
-- Create a value of 'CreateDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingSelectionExpression', 'createDomainNameResponse_apiMappingSelectionExpression' - The API mapping selection expression.
--
-- 'domainName', 'createDomainNameResponse_domainName' - The name of the DomainName resource.
--
-- 'domainNameConfigurations', 'createDomainNameResponse_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'createDomainNameResponse_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'tags', 'createDomainNameResponse_tags' - The collection of tags associated with a domain name.
--
-- 'httpStatus', 'createDomainNameResponse_httpStatus' - The response's http status code.
newCreateDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainNameResponse
newCreateDomainNameResponse pHttpStatus_ =
  CreateDomainNameResponse'
    { apiMappingSelectionExpression =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainNameConfigurations = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API mapping selection expression.
createDomainNameResponse_apiMappingSelectionExpression :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe Prelude.Text)
createDomainNameResponse_apiMappingSelectionExpression = Lens.lens (\CreateDomainNameResponse' {apiMappingSelectionExpression} -> apiMappingSelectionExpression) (\s@CreateDomainNameResponse' {} a -> s {apiMappingSelectionExpression = a} :: CreateDomainNameResponse)

-- | The name of the DomainName resource.
createDomainNameResponse_domainName :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe Prelude.Text)
createDomainNameResponse_domainName = Lens.lens (\CreateDomainNameResponse' {domainName} -> domainName) (\s@CreateDomainNameResponse' {} a -> s {domainName = a} :: CreateDomainNameResponse)

-- | The domain name configurations.
createDomainNameResponse_domainNameConfigurations :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe [DomainNameConfiguration])
createDomainNameResponse_domainNameConfigurations = Lens.lens (\CreateDomainNameResponse' {domainNameConfigurations} -> domainNameConfigurations) (\s@CreateDomainNameResponse' {} a -> s {domainNameConfigurations = a} :: CreateDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
createDomainNameResponse_mutualTlsAuthentication :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe MutualTlsAuthentication)
createDomainNameResponse_mutualTlsAuthentication = Lens.lens (\CreateDomainNameResponse' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@CreateDomainNameResponse' {} a -> s {mutualTlsAuthentication = a} :: CreateDomainNameResponse)

-- | The collection of tags associated with a domain name.
createDomainNameResponse_tags :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainNameResponse_tags = Lens.lens (\CreateDomainNameResponse' {tags} -> tags) (\s@CreateDomainNameResponse' {} a -> s {tags = a} :: CreateDomainNameResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createDomainNameResponse_httpStatus :: Lens.Lens' CreateDomainNameResponse Prelude.Int
createDomainNameResponse_httpStatus = Lens.lens (\CreateDomainNameResponse' {httpStatus} -> httpStatus) (\s@CreateDomainNameResponse' {} a -> s {httpStatus = a} :: CreateDomainNameResponse)

instance Prelude.NFData CreateDomainNameResponse where
  rnf CreateDomainNameResponse' {..} =
    Prelude.rnf apiMappingSelectionExpression
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

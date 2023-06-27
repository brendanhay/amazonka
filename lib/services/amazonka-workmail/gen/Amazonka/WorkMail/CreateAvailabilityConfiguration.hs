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
-- Module      : Amazonka.WorkMail.CreateAvailabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an @AvailabilityConfiguration@ for the given WorkMail
-- organization and domain.
module Amazonka.WorkMail.CreateAvailabilityConfiguration
  ( -- * Creating a Request
    CreateAvailabilityConfiguration (..),
    newCreateAvailabilityConfiguration,

    -- * Request Lenses
    createAvailabilityConfiguration_clientToken,
    createAvailabilityConfiguration_ewsProvider,
    createAvailabilityConfiguration_lambdaProvider,
    createAvailabilityConfiguration_organizationId,
    createAvailabilityConfiguration_domainName,

    -- * Destructuring the Response
    CreateAvailabilityConfigurationResponse (..),
    newCreateAvailabilityConfigurationResponse,

    -- * Response Lenses
    createAvailabilityConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newCreateAvailabilityConfiguration' smart constructor.
data CreateAvailabilityConfiguration = CreateAvailabilityConfiguration'
  { -- | An idempotent token that ensures that an API request is executed only
    -- once.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Exchange Web Services (EWS) availability provider definition. The
    -- request must contain exactly one provider definition, either
    -- @EwsProvider@ or @LambdaProvider@.
    ewsProvider :: Prelude.Maybe EwsAvailabilityProvider,
    -- | Lambda availability provider definition. The request must contain
    -- exactly one provider definition, either @EwsProvider@ or
    -- @LambdaProvider@.
    lambdaProvider :: Prelude.Maybe LambdaAvailabilityProvider,
    -- | The WorkMail organization for which the @AvailabilityConfiguration@ will
    -- be created.
    organizationId :: Prelude.Text,
    -- | The domain to which the provider applies.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAvailabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAvailabilityConfiguration_clientToken' - An idempotent token that ensures that an API request is executed only
-- once.
--
-- 'ewsProvider', 'createAvailabilityConfiguration_ewsProvider' - Exchange Web Services (EWS) availability provider definition. The
-- request must contain exactly one provider definition, either
-- @EwsProvider@ or @LambdaProvider@.
--
-- 'lambdaProvider', 'createAvailabilityConfiguration_lambdaProvider' - Lambda availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@.
--
-- 'organizationId', 'createAvailabilityConfiguration_organizationId' - The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be created.
--
-- 'domainName', 'createAvailabilityConfiguration_domainName' - The domain to which the provider applies.
newCreateAvailabilityConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  CreateAvailabilityConfiguration
newCreateAvailabilityConfiguration
  pOrganizationId_
  pDomainName_ =
    CreateAvailabilityConfiguration'
      { clientToken =
          Prelude.Nothing,
        ewsProvider = Prelude.Nothing,
        lambdaProvider = Prelude.Nothing,
        organizationId = pOrganizationId_,
        domainName = pDomainName_
      }

-- | An idempotent token that ensures that an API request is executed only
-- once.
createAvailabilityConfiguration_clientToken :: Lens.Lens' CreateAvailabilityConfiguration (Prelude.Maybe Prelude.Text)
createAvailabilityConfiguration_clientToken = Lens.lens (\CreateAvailabilityConfiguration' {clientToken} -> clientToken) (\s@CreateAvailabilityConfiguration' {} a -> s {clientToken = a} :: CreateAvailabilityConfiguration)

-- | Exchange Web Services (EWS) availability provider definition. The
-- request must contain exactly one provider definition, either
-- @EwsProvider@ or @LambdaProvider@.
createAvailabilityConfiguration_ewsProvider :: Lens.Lens' CreateAvailabilityConfiguration (Prelude.Maybe EwsAvailabilityProvider)
createAvailabilityConfiguration_ewsProvider = Lens.lens (\CreateAvailabilityConfiguration' {ewsProvider} -> ewsProvider) (\s@CreateAvailabilityConfiguration' {} a -> s {ewsProvider = a} :: CreateAvailabilityConfiguration)

-- | Lambda availability provider definition. The request must contain
-- exactly one provider definition, either @EwsProvider@ or
-- @LambdaProvider@.
createAvailabilityConfiguration_lambdaProvider :: Lens.Lens' CreateAvailabilityConfiguration (Prelude.Maybe LambdaAvailabilityProvider)
createAvailabilityConfiguration_lambdaProvider = Lens.lens (\CreateAvailabilityConfiguration' {lambdaProvider} -> lambdaProvider) (\s@CreateAvailabilityConfiguration' {} a -> s {lambdaProvider = a} :: CreateAvailabilityConfiguration)

-- | The WorkMail organization for which the @AvailabilityConfiguration@ will
-- be created.
createAvailabilityConfiguration_organizationId :: Lens.Lens' CreateAvailabilityConfiguration Prelude.Text
createAvailabilityConfiguration_organizationId = Lens.lens (\CreateAvailabilityConfiguration' {organizationId} -> organizationId) (\s@CreateAvailabilityConfiguration' {} a -> s {organizationId = a} :: CreateAvailabilityConfiguration)

-- | The domain to which the provider applies.
createAvailabilityConfiguration_domainName :: Lens.Lens' CreateAvailabilityConfiguration Prelude.Text
createAvailabilityConfiguration_domainName = Lens.lens (\CreateAvailabilityConfiguration' {domainName} -> domainName) (\s@CreateAvailabilityConfiguration' {} a -> s {domainName = a} :: CreateAvailabilityConfiguration)

instance
  Core.AWSRequest
    CreateAvailabilityConfiguration
  where
  type
    AWSResponse CreateAvailabilityConfiguration =
      CreateAvailabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAvailabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateAvailabilityConfiguration
  where
  hashWithSalt
    _salt
    CreateAvailabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` ewsProvider
        `Prelude.hashWithSalt` lambdaProvider
        `Prelude.hashWithSalt` organizationId
        `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    CreateAvailabilityConfiguration
  where
  rnf CreateAvailabilityConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf ewsProvider
      `Prelude.seq` Prelude.rnf lambdaProvider
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance
  Data.ToHeaders
    CreateAvailabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.CreateAvailabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAvailabilityConfiguration where
  toJSON CreateAvailabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("EwsProvider" Data..=) Prelude.<$> ewsProvider,
            ("LambdaProvider" Data..=)
              Prelude.<$> lambdaProvider,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateAvailabilityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAvailabilityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAvailabilityConfigurationResponse' smart constructor.
data CreateAvailabilityConfigurationResponse = CreateAvailabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAvailabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAvailabilityConfigurationResponse_httpStatus' - The response's http status code.
newCreateAvailabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAvailabilityConfigurationResponse
newCreateAvailabilityConfigurationResponse
  pHttpStatus_ =
    CreateAvailabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createAvailabilityConfigurationResponse_httpStatus :: Lens.Lens' CreateAvailabilityConfigurationResponse Prelude.Int
createAvailabilityConfigurationResponse_httpStatus = Lens.lens (\CreateAvailabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateAvailabilityConfigurationResponse' {} a -> s {httpStatus = a} :: CreateAvailabilityConfigurationResponse)

instance
  Prelude.NFData
    CreateAvailabilityConfigurationResponse
  where
  rnf CreateAvailabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.WorkLink.AssociateWebsiteAuthorizationProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a website authorization provider with a specified fleet. This
-- is used to authorize users against associated websites in the company
-- network.
module Amazonka.WorkLink.AssociateWebsiteAuthorizationProvider
  ( -- * Creating a Request
    AssociateWebsiteAuthorizationProvider (..),
    newAssociateWebsiteAuthorizationProvider,

    -- * Request Lenses
    associateWebsiteAuthorizationProvider_domainName,
    associateWebsiteAuthorizationProvider_fleetArn,
    associateWebsiteAuthorizationProvider_authorizationProviderType,

    -- * Destructuring the Response
    AssociateWebsiteAuthorizationProviderResponse (..),
    newAssociateWebsiteAuthorizationProviderResponse,

    -- * Response Lenses
    associateWebsiteAuthorizationProviderResponse_authorizationProviderId,
    associateWebsiteAuthorizationProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newAssociateWebsiteAuthorizationProvider' smart constructor.
data AssociateWebsiteAuthorizationProvider = AssociateWebsiteAuthorizationProvider'
  { -- | The domain name of the authorization provider. This applies only to
    -- SAML-based authorization providers.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The authorization provider type.
    authorizationProviderType :: AuthorizationProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWebsiteAuthorizationProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'associateWebsiteAuthorizationProvider_domainName' - The domain name of the authorization provider. This applies only to
-- SAML-based authorization providers.
--
-- 'fleetArn', 'associateWebsiteAuthorizationProvider_fleetArn' - The ARN of the fleet.
--
-- 'authorizationProviderType', 'associateWebsiteAuthorizationProvider_authorizationProviderType' - The authorization provider type.
newAssociateWebsiteAuthorizationProvider ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'authorizationProviderType'
  AuthorizationProviderType ->
  AssociateWebsiteAuthorizationProvider
newAssociateWebsiteAuthorizationProvider
  pFleetArn_
  pAuthorizationProviderType_ =
    AssociateWebsiteAuthorizationProvider'
      { domainName =
          Prelude.Nothing,
        fleetArn = pFleetArn_,
        authorizationProviderType =
          pAuthorizationProviderType_
      }

-- | The domain name of the authorization provider. This applies only to
-- SAML-based authorization providers.
associateWebsiteAuthorizationProvider_domainName :: Lens.Lens' AssociateWebsiteAuthorizationProvider (Prelude.Maybe Prelude.Text)
associateWebsiteAuthorizationProvider_domainName = Lens.lens (\AssociateWebsiteAuthorizationProvider' {domainName} -> domainName) (\s@AssociateWebsiteAuthorizationProvider' {} a -> s {domainName = a} :: AssociateWebsiteAuthorizationProvider)

-- | The ARN of the fleet.
associateWebsiteAuthorizationProvider_fleetArn :: Lens.Lens' AssociateWebsiteAuthorizationProvider Prelude.Text
associateWebsiteAuthorizationProvider_fleetArn = Lens.lens (\AssociateWebsiteAuthorizationProvider' {fleetArn} -> fleetArn) (\s@AssociateWebsiteAuthorizationProvider' {} a -> s {fleetArn = a} :: AssociateWebsiteAuthorizationProvider)

-- | The authorization provider type.
associateWebsiteAuthorizationProvider_authorizationProviderType :: Lens.Lens' AssociateWebsiteAuthorizationProvider AuthorizationProviderType
associateWebsiteAuthorizationProvider_authorizationProviderType = Lens.lens (\AssociateWebsiteAuthorizationProvider' {authorizationProviderType} -> authorizationProviderType) (\s@AssociateWebsiteAuthorizationProvider' {} a -> s {authorizationProviderType = a} :: AssociateWebsiteAuthorizationProvider)

instance
  Core.AWSRequest
    AssociateWebsiteAuthorizationProvider
  where
  type
    AWSResponse
      AssociateWebsiteAuthorizationProvider =
      AssociateWebsiteAuthorizationProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateWebsiteAuthorizationProviderResponse'
            Prelude.<$> (x Core..?> "AuthorizationProviderId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWebsiteAuthorizationProvider
  where
  hashWithSalt
    _salt
    AssociateWebsiteAuthorizationProvider' {..} =
      _salt `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` fleetArn
        `Prelude.hashWithSalt` authorizationProviderType

instance
  Prelude.NFData
    AssociateWebsiteAuthorizationProvider
  where
  rnf AssociateWebsiteAuthorizationProvider' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf authorizationProviderType

instance
  Core.ToHeaders
    AssociateWebsiteAuthorizationProvider
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    AssociateWebsiteAuthorizationProvider
  where
  toJSON AssociateWebsiteAuthorizationProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DomainName" Core..=) Prelude.<$> domainName,
            Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just
              ( "AuthorizationProviderType"
                  Core..= authorizationProviderType
              )
          ]
      )

instance
  Core.ToPath
    AssociateWebsiteAuthorizationProvider
  where
  toPath =
    Prelude.const
      "/associateWebsiteAuthorizationProvider"

instance
  Core.ToQuery
    AssociateWebsiteAuthorizationProvider
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWebsiteAuthorizationProviderResponse' smart constructor.
data AssociateWebsiteAuthorizationProviderResponse = AssociateWebsiteAuthorizationProviderResponse'
  { -- | A unique identifier for the authorization provider.
    authorizationProviderId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWebsiteAuthorizationProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationProviderId', 'associateWebsiteAuthorizationProviderResponse_authorizationProviderId' - A unique identifier for the authorization provider.
--
-- 'httpStatus', 'associateWebsiteAuthorizationProviderResponse_httpStatus' - The response's http status code.
newAssociateWebsiteAuthorizationProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWebsiteAuthorizationProviderResponse
newAssociateWebsiteAuthorizationProviderResponse
  pHttpStatus_ =
    AssociateWebsiteAuthorizationProviderResponse'
      { authorizationProviderId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A unique identifier for the authorization provider.
associateWebsiteAuthorizationProviderResponse_authorizationProviderId :: Lens.Lens' AssociateWebsiteAuthorizationProviderResponse (Prelude.Maybe Prelude.Text)
associateWebsiteAuthorizationProviderResponse_authorizationProviderId = Lens.lens (\AssociateWebsiteAuthorizationProviderResponse' {authorizationProviderId} -> authorizationProviderId) (\s@AssociateWebsiteAuthorizationProviderResponse' {} a -> s {authorizationProviderId = a} :: AssociateWebsiteAuthorizationProviderResponse)

-- | The response's http status code.
associateWebsiteAuthorizationProviderResponse_httpStatus :: Lens.Lens' AssociateWebsiteAuthorizationProviderResponse Prelude.Int
associateWebsiteAuthorizationProviderResponse_httpStatus = Lens.lens (\AssociateWebsiteAuthorizationProviderResponse' {httpStatus} -> httpStatus) (\s@AssociateWebsiteAuthorizationProviderResponse' {} a -> s {httpStatus = a} :: AssociateWebsiteAuthorizationProviderResponse)

instance
  Prelude.NFData
    AssociateWebsiteAuthorizationProviderResponse
  where
  rnf
    AssociateWebsiteAuthorizationProviderResponse' {..} =
      Prelude.rnf authorizationProviderId
        `Prelude.seq` Prelude.rnf httpStatus

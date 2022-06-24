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
-- Module      : Amazonka.WorkLink.DescribeIdentityProviderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the identity provider configuration of the specified fleet.
module Amazonka.WorkLink.DescribeIdentityProviderConfiguration
  ( -- * Creating a Request
    DescribeIdentityProviderConfiguration (..),
    newDescribeIdentityProviderConfiguration,

    -- * Request Lenses
    describeIdentityProviderConfiguration_fleetArn,

    -- * Destructuring the Response
    DescribeIdentityProviderConfigurationResponse (..),
    newDescribeIdentityProviderConfigurationResponse,

    -- * Response Lenses
    describeIdentityProviderConfigurationResponse_identityProviderSamlMetadata,
    describeIdentityProviderConfigurationResponse_identityProviderType,
    describeIdentityProviderConfigurationResponse_serviceProviderSamlMetadata,
    describeIdentityProviderConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDescribeIdentityProviderConfiguration' smart constructor.
data DescribeIdentityProviderConfiguration = DescribeIdentityProviderConfiguration'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityProviderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeIdentityProviderConfiguration_fleetArn' - The ARN of the fleet.
newDescribeIdentityProviderConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  DescribeIdentityProviderConfiguration
newDescribeIdentityProviderConfiguration pFleetArn_ =
  DescribeIdentityProviderConfiguration'
    { fleetArn =
        pFleetArn_
    }

-- | The ARN of the fleet.
describeIdentityProviderConfiguration_fleetArn :: Lens.Lens' DescribeIdentityProviderConfiguration Prelude.Text
describeIdentityProviderConfiguration_fleetArn = Lens.lens (\DescribeIdentityProviderConfiguration' {fleetArn} -> fleetArn) (\s@DescribeIdentityProviderConfiguration' {} a -> s {fleetArn = a} :: DescribeIdentityProviderConfiguration)

instance
  Core.AWSRequest
    DescribeIdentityProviderConfiguration
  where
  type
    AWSResponse
      DescribeIdentityProviderConfiguration =
      DescribeIdentityProviderConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderConfigurationResponse'
            Prelude.<$> (x Core..?> "IdentityProviderSamlMetadata")
              Prelude.<*> (x Core..?> "IdentityProviderType")
              Prelude.<*> (x Core..?> "ServiceProviderSamlMetadata")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeIdentityProviderConfiguration
  where
  hashWithSalt
    _salt
    DescribeIdentityProviderConfiguration' {..} =
      _salt `Prelude.hashWithSalt` fleetArn

instance
  Prelude.NFData
    DescribeIdentityProviderConfiguration
  where
  rnf DescribeIdentityProviderConfiguration' {..} =
    Prelude.rnf fleetArn

instance
  Core.ToHeaders
    DescribeIdentityProviderConfiguration
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
    DescribeIdentityProviderConfiguration
  where
  toJSON DescribeIdentityProviderConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance
  Core.ToPath
    DescribeIdentityProviderConfiguration
  where
  toPath =
    Prelude.const
      "/describeIdentityProviderConfiguration"

instance
  Core.ToQuery
    DescribeIdentityProviderConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIdentityProviderConfigurationResponse' smart constructor.
data DescribeIdentityProviderConfigurationResponse = DescribeIdentityProviderConfigurationResponse'
  { -- | The SAML metadata document provided by the user’s identity provider.
    identityProviderSamlMetadata :: Prelude.Maybe Prelude.Text,
    -- | The type of identity provider.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | The SAML metadata document uploaded to the user’s identity provider.
    serviceProviderSamlMetadata :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityProviderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderSamlMetadata', 'describeIdentityProviderConfigurationResponse_identityProviderSamlMetadata' - The SAML metadata document provided by the user’s identity provider.
--
-- 'identityProviderType', 'describeIdentityProviderConfigurationResponse_identityProviderType' - The type of identity provider.
--
-- 'serviceProviderSamlMetadata', 'describeIdentityProviderConfigurationResponse_serviceProviderSamlMetadata' - The SAML metadata document uploaded to the user’s identity provider.
--
-- 'httpStatus', 'describeIdentityProviderConfigurationResponse_httpStatus' - The response's http status code.
newDescribeIdentityProviderConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIdentityProviderConfigurationResponse
newDescribeIdentityProviderConfigurationResponse
  pHttpStatus_ =
    DescribeIdentityProviderConfigurationResponse'
      { identityProviderSamlMetadata =
          Prelude.Nothing,
        identityProviderType =
          Prelude.Nothing,
        serviceProviderSamlMetadata =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The SAML metadata document provided by the user’s identity provider.
describeIdentityProviderConfigurationResponse_identityProviderSamlMetadata :: Lens.Lens' DescribeIdentityProviderConfigurationResponse (Prelude.Maybe Prelude.Text)
describeIdentityProviderConfigurationResponse_identityProviderSamlMetadata = Lens.lens (\DescribeIdentityProviderConfigurationResponse' {identityProviderSamlMetadata} -> identityProviderSamlMetadata) (\s@DescribeIdentityProviderConfigurationResponse' {} a -> s {identityProviderSamlMetadata = a} :: DescribeIdentityProviderConfigurationResponse)

-- | The type of identity provider.
describeIdentityProviderConfigurationResponse_identityProviderType :: Lens.Lens' DescribeIdentityProviderConfigurationResponse (Prelude.Maybe IdentityProviderType)
describeIdentityProviderConfigurationResponse_identityProviderType = Lens.lens (\DescribeIdentityProviderConfigurationResponse' {identityProviderType} -> identityProviderType) (\s@DescribeIdentityProviderConfigurationResponse' {} a -> s {identityProviderType = a} :: DescribeIdentityProviderConfigurationResponse)

-- | The SAML metadata document uploaded to the user’s identity provider.
describeIdentityProviderConfigurationResponse_serviceProviderSamlMetadata :: Lens.Lens' DescribeIdentityProviderConfigurationResponse (Prelude.Maybe Prelude.Text)
describeIdentityProviderConfigurationResponse_serviceProviderSamlMetadata = Lens.lens (\DescribeIdentityProviderConfigurationResponse' {serviceProviderSamlMetadata} -> serviceProviderSamlMetadata) (\s@DescribeIdentityProviderConfigurationResponse' {} a -> s {serviceProviderSamlMetadata = a} :: DescribeIdentityProviderConfigurationResponse)

-- | The response's http status code.
describeIdentityProviderConfigurationResponse_httpStatus :: Lens.Lens' DescribeIdentityProviderConfigurationResponse Prelude.Int
describeIdentityProviderConfigurationResponse_httpStatus = Lens.lens (\DescribeIdentityProviderConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityProviderConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeIdentityProviderConfigurationResponse)

instance
  Prelude.NFData
    DescribeIdentityProviderConfigurationResponse
  where
  rnf
    DescribeIdentityProviderConfigurationResponse' {..} =
      Prelude.rnf identityProviderSamlMetadata
        `Prelude.seq` Prelude.rnf identityProviderType
        `Prelude.seq` Prelude.rnf serviceProviderSamlMetadata
        `Prelude.seq` Prelude.rnf httpStatus

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextSdsTrust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual gateway\'s listener\'s Transport
-- Layer Security (TLS) Secret Discovery Service validation context trust.
-- The proxy must be configured with a local SDS provider via a Unix Domain
-- Socket. See App Mesh
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html TLS documentation>
-- for more info.
--
-- /See:/ 'newVirtualGatewayTlsValidationContextSdsTrust' smart constructor.
data VirtualGatewayTlsValidationContextSdsTrust = VirtualGatewayTlsValidationContextSdsTrust'
  { -- | A reference to an object that represents the name of the secret for a
    -- virtual gateway\'s Transport Layer Security (TLS) Secret Discovery
    -- Service validation context trust.
    secretName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayTlsValidationContextSdsTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretName', 'virtualGatewayTlsValidationContextSdsTrust_secretName' - A reference to an object that represents the name of the secret for a
-- virtual gateway\'s Transport Layer Security (TLS) Secret Discovery
-- Service validation context trust.
newVirtualGatewayTlsValidationContextSdsTrust ::
  -- | 'secretName'
  Prelude.Text ->
  VirtualGatewayTlsValidationContextSdsTrust
newVirtualGatewayTlsValidationContextSdsTrust
  pSecretName_ =
    VirtualGatewayTlsValidationContextSdsTrust'
      { secretName =
          pSecretName_
      }

-- | A reference to an object that represents the name of the secret for a
-- virtual gateway\'s Transport Layer Security (TLS) Secret Discovery
-- Service validation context trust.
virtualGatewayTlsValidationContextSdsTrust_secretName :: Lens.Lens' VirtualGatewayTlsValidationContextSdsTrust Prelude.Text
virtualGatewayTlsValidationContextSdsTrust_secretName = Lens.lens (\VirtualGatewayTlsValidationContextSdsTrust' {secretName} -> secretName) (\s@VirtualGatewayTlsValidationContextSdsTrust' {} a -> s {secretName = a} :: VirtualGatewayTlsValidationContextSdsTrust)

instance
  Core.FromJSON
    VirtualGatewayTlsValidationContextSdsTrust
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayTlsValidationContextSdsTrust"
      ( \x ->
          VirtualGatewayTlsValidationContextSdsTrust'
            Prelude.<$> (x Core..: "secretName")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContextSdsTrust
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContextSdsTrust' {..} =
      _salt `Prelude.hashWithSalt` secretName

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContextSdsTrust
  where
  rnf VirtualGatewayTlsValidationContextSdsTrust' {..} =
    Prelude.rnf secretName

instance
  Core.ToJSON
    VirtualGatewayTlsValidationContextSdsTrust
  where
  toJSON
    VirtualGatewayTlsValidationContextSdsTrust' {..} =
      Core.object
        ( Prelude.catMaybes
            [Prelude.Just ("secretName" Core..= secretName)]
        )

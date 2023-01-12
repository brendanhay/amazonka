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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the virtual gateway\'s listener\'s Secret
-- Discovery Service certificate.The proxy must be configured with a local
-- SDS provider via a Unix Domain Socket. See App
-- Mesh<https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html TLS documentation>
-- for more info.
--
-- /See:/ 'newVirtualGatewayListenerTlsSdsCertificate' smart constructor.
data VirtualGatewayListenerTlsSdsCertificate = VirtualGatewayListenerTlsSdsCertificate'
  { -- | A reference to an object that represents the name of the secret secret
    -- requested from the Secret Discovery Service provider representing
    -- Transport Layer Security (TLS) materials like a certificate or
    -- certificate chain.
    secretName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsSdsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretName', 'virtualGatewayListenerTlsSdsCertificate_secretName' - A reference to an object that represents the name of the secret secret
-- requested from the Secret Discovery Service provider representing
-- Transport Layer Security (TLS) materials like a certificate or
-- certificate chain.
newVirtualGatewayListenerTlsSdsCertificate ::
  -- | 'secretName'
  Prelude.Text ->
  VirtualGatewayListenerTlsSdsCertificate
newVirtualGatewayListenerTlsSdsCertificate
  pSecretName_ =
    VirtualGatewayListenerTlsSdsCertificate'
      { secretName =
          pSecretName_
      }

-- | A reference to an object that represents the name of the secret secret
-- requested from the Secret Discovery Service provider representing
-- Transport Layer Security (TLS) materials like a certificate or
-- certificate chain.
virtualGatewayListenerTlsSdsCertificate_secretName :: Lens.Lens' VirtualGatewayListenerTlsSdsCertificate Prelude.Text
virtualGatewayListenerTlsSdsCertificate_secretName = Lens.lens (\VirtualGatewayListenerTlsSdsCertificate' {secretName} -> secretName) (\s@VirtualGatewayListenerTlsSdsCertificate' {} a -> s {secretName = a} :: VirtualGatewayListenerTlsSdsCertificate)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsSdsCertificate
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsSdsCertificate"
      ( \x ->
          VirtualGatewayListenerTlsSdsCertificate'
            Prelude.<$> (x Data..: "secretName")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsSdsCertificate
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsSdsCertificate' {..} =
      _salt `Prelude.hashWithSalt` secretName

instance
  Prelude.NFData
    VirtualGatewayListenerTlsSdsCertificate
  where
  rnf VirtualGatewayListenerTlsSdsCertificate' {..} =
    Prelude.rnf secretName

instance
  Data.ToJSON
    VirtualGatewayListenerTlsSdsCertificate
  where
  toJSON VirtualGatewayListenerTlsSdsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("secretName" Data..= secretName)]
      )

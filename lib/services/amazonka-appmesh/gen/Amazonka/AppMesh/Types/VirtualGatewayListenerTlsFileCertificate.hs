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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
--
-- /See:/ 'newVirtualGatewayListenerTlsFileCertificate' smart constructor.
data VirtualGatewayListenerTlsFileCertificate = VirtualGatewayListenerTlsFileCertificate'
  { -- | The certificate chain for the certificate.
    certificateChain :: Prelude.Text,
    -- | The private key for a certificate stored on the file system of the mesh
    -- endpoint that the proxy is running on.
    privateKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsFileCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'virtualGatewayListenerTlsFileCertificate_certificateChain' - The certificate chain for the certificate.
--
-- 'privateKey', 'virtualGatewayListenerTlsFileCertificate_privateKey' - The private key for a certificate stored on the file system of the mesh
-- endpoint that the proxy is running on.
newVirtualGatewayListenerTlsFileCertificate ::
  -- | 'certificateChain'
  Prelude.Text ->
  -- | 'privateKey'
  Prelude.Text ->
  VirtualGatewayListenerTlsFileCertificate
newVirtualGatewayListenerTlsFileCertificate
  pCertificateChain_
  pPrivateKey_ =
    VirtualGatewayListenerTlsFileCertificate'
      { certificateChain =
          pCertificateChain_,
        privateKey = pPrivateKey_
      }

-- | The certificate chain for the certificate.
virtualGatewayListenerTlsFileCertificate_certificateChain :: Lens.Lens' VirtualGatewayListenerTlsFileCertificate Prelude.Text
virtualGatewayListenerTlsFileCertificate_certificateChain = Lens.lens (\VirtualGatewayListenerTlsFileCertificate' {certificateChain} -> certificateChain) (\s@VirtualGatewayListenerTlsFileCertificate' {} a -> s {certificateChain = a} :: VirtualGatewayListenerTlsFileCertificate)

-- | The private key for a certificate stored on the file system of the mesh
-- endpoint that the proxy is running on.
virtualGatewayListenerTlsFileCertificate_privateKey :: Lens.Lens' VirtualGatewayListenerTlsFileCertificate Prelude.Text
virtualGatewayListenerTlsFileCertificate_privateKey = Lens.lens (\VirtualGatewayListenerTlsFileCertificate' {privateKey} -> privateKey) (\s@VirtualGatewayListenerTlsFileCertificate' {} a -> s {privateKey = a} :: VirtualGatewayListenerTlsFileCertificate)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsFileCertificate
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsFileCertificate"
      ( \x ->
          VirtualGatewayListenerTlsFileCertificate'
            Prelude.<$> (x Data..: "certificateChain")
            Prelude.<*> (x Data..: "privateKey")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsFileCertificate
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsFileCertificate' {..} =
      _salt
        `Prelude.hashWithSalt` certificateChain
        `Prelude.hashWithSalt` privateKey

instance
  Prelude.NFData
    VirtualGatewayListenerTlsFileCertificate
  where
  rnf VirtualGatewayListenerTlsFileCertificate' {..} =
    Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf privateKey

instance
  Data.ToJSON
    VirtualGatewayListenerTlsFileCertificate
  where
  toJSON VirtualGatewayListenerTlsFileCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateChain" Data..= certificateChain),
            Prelude.Just ("privateKey" Data..= privateKey)
          ]
      )

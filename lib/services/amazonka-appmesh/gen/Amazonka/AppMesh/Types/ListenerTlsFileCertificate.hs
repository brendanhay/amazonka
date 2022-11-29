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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsFileCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsFileCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
--
-- /See:/ 'newListenerTlsFileCertificate' smart constructor.
data ListenerTlsFileCertificate = ListenerTlsFileCertificate'
  { -- | The certificate chain for the certificate.
    certificateChain :: Prelude.Text,
    -- | The private key for a certificate stored on the file system of the
    -- virtual node that the proxy is running on.
    privateKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsFileCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'listenerTlsFileCertificate_certificateChain' - The certificate chain for the certificate.
--
-- 'privateKey', 'listenerTlsFileCertificate_privateKey' - The private key for a certificate stored on the file system of the
-- virtual node that the proxy is running on.
newListenerTlsFileCertificate ::
  -- | 'certificateChain'
  Prelude.Text ->
  -- | 'privateKey'
  Prelude.Text ->
  ListenerTlsFileCertificate
newListenerTlsFileCertificate
  pCertificateChain_
  pPrivateKey_ =
    ListenerTlsFileCertificate'
      { certificateChain =
          pCertificateChain_,
        privateKey = pPrivateKey_
      }

-- | The certificate chain for the certificate.
listenerTlsFileCertificate_certificateChain :: Lens.Lens' ListenerTlsFileCertificate Prelude.Text
listenerTlsFileCertificate_certificateChain = Lens.lens (\ListenerTlsFileCertificate' {certificateChain} -> certificateChain) (\s@ListenerTlsFileCertificate' {} a -> s {certificateChain = a} :: ListenerTlsFileCertificate)

-- | The private key for a certificate stored on the file system of the
-- virtual node that the proxy is running on.
listenerTlsFileCertificate_privateKey :: Lens.Lens' ListenerTlsFileCertificate Prelude.Text
listenerTlsFileCertificate_privateKey = Lens.lens (\ListenerTlsFileCertificate' {privateKey} -> privateKey) (\s@ListenerTlsFileCertificate' {} a -> s {privateKey = a} :: ListenerTlsFileCertificate)

instance Core.FromJSON ListenerTlsFileCertificate where
  parseJSON =
    Core.withObject
      "ListenerTlsFileCertificate"
      ( \x ->
          ListenerTlsFileCertificate'
            Prelude.<$> (x Core..: "certificateChain")
            Prelude.<*> (x Core..: "privateKey")
      )

instance Prelude.Hashable ListenerTlsFileCertificate where
  hashWithSalt _salt ListenerTlsFileCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` privateKey

instance Prelude.NFData ListenerTlsFileCertificate where
  rnf ListenerTlsFileCertificate' {..} =
    Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf privateKey

instance Core.ToJSON ListenerTlsFileCertificate where
  toJSON ListenerTlsFileCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateChain" Core..= certificateChain),
            Prelude.Just ("privateKey" Core..= privateKey)
          ]
      )

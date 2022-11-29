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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsAcmCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsAcmCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Certificate Manager certificate.
--
-- /See:/ 'newListenerTlsAcmCertificate' smart constructor.
data ListenerTlsAcmCertificate = ListenerTlsAcmCertificate'
  { -- | The Amazon Resource Name (ARN) for the certificate. The certificate must
    -- meet specific requirements and you must have proxy authorization
    -- enabled. For more information, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsAcmCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'listenerTlsAcmCertificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
newListenerTlsAcmCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  ListenerTlsAcmCertificate
newListenerTlsAcmCertificate pCertificateArn_ =
  ListenerTlsAcmCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | The Amazon Resource Name (ARN) for the certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
listenerTlsAcmCertificate_certificateArn :: Lens.Lens' ListenerTlsAcmCertificate Prelude.Text
listenerTlsAcmCertificate_certificateArn = Lens.lens (\ListenerTlsAcmCertificate' {certificateArn} -> certificateArn) (\s@ListenerTlsAcmCertificate' {} a -> s {certificateArn = a} :: ListenerTlsAcmCertificate)

instance Core.FromJSON ListenerTlsAcmCertificate where
  parseJSON =
    Core.withObject
      "ListenerTlsAcmCertificate"
      ( \x ->
          ListenerTlsAcmCertificate'
            Prelude.<$> (x Core..: "certificateArn")
      )

instance Prelude.Hashable ListenerTlsAcmCertificate where
  hashWithSalt _salt ListenerTlsAcmCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData ListenerTlsAcmCertificate where
  rnf ListenerTlsAcmCertificate' {..} =
    Prelude.rnf certificateArn

instance Core.ToJSON ListenerTlsAcmCertificate where
  toJSON ListenerTlsAcmCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateArn" Core..= certificateArn)
          ]
      )

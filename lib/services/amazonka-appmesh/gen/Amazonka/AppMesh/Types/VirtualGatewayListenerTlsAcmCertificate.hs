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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Certificate Manager certificate.
--
-- /See:/ 'newVirtualGatewayListenerTlsAcmCertificate' smart constructor.
data VirtualGatewayListenerTlsAcmCertificate = VirtualGatewayListenerTlsAcmCertificate'
  { -- | The Amazon Resource Name (ARN) for the certificate. The certificate must
    -- meet specific requirements and you must have proxy authorization
    -- enabled. For more information, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsAcmCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'virtualGatewayListenerTlsAcmCertificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
newVirtualGatewayListenerTlsAcmCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  VirtualGatewayListenerTlsAcmCertificate
newVirtualGatewayListenerTlsAcmCertificate
  pCertificateArn_ =
    VirtualGatewayListenerTlsAcmCertificate'
      { certificateArn =
          pCertificateArn_
      }

-- | The Amazon Resource Name (ARN) for the certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html#virtual-node-tls-prerequisites Transport Layer Security (TLS)>.
virtualGatewayListenerTlsAcmCertificate_certificateArn :: Lens.Lens' VirtualGatewayListenerTlsAcmCertificate Prelude.Text
virtualGatewayListenerTlsAcmCertificate_certificateArn = Lens.lens (\VirtualGatewayListenerTlsAcmCertificate' {certificateArn} -> certificateArn) (\s@VirtualGatewayListenerTlsAcmCertificate' {} a -> s {certificateArn = a} :: VirtualGatewayListenerTlsAcmCertificate)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsAcmCertificate
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsAcmCertificate"
      ( \x ->
          VirtualGatewayListenerTlsAcmCertificate'
            Prelude.<$> (x Data..: "certificateArn")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsAcmCertificate
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsAcmCertificate' {..} =
      _salt `Prelude.hashWithSalt` certificateArn

instance
  Prelude.NFData
    VirtualGatewayListenerTlsAcmCertificate
  where
  rnf VirtualGatewayListenerTlsAcmCertificate' {..} =
    Prelude.rnf certificateArn

instance
  Data.ToJSON
    VirtualGatewayListenerTlsAcmCertificate
  where
  toJSON VirtualGatewayListenerTlsAcmCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateArn" Data..= certificateArn)
          ]
      )

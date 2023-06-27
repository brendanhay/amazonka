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
-- Module      : Amazonka.NetworkFirewall.Types.ServerCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.ServerCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Any Certificate Manager Secure Sockets Layer\/Transport Layer Security
-- (SSL\/TLS) server certificate that\'s associated with a
-- ServerCertificateConfiguration used in a TLSInspectionConfiguration. You
-- must request or import a SSL\/TLS certificate into ACM for each domain
-- Network Firewall needs to decrypt and inspect. Network Firewall uses the
-- SSL\/TLS certificates to decrypt specified inbound SSL\/TLS traffic
-- going to your firewall. For information about working with certificates
-- in Certificate Manager, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html Request a public certificate>
-- or
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing certificates>
-- in the /Certificate Manager User Guide/.
--
-- /See:/ 'newServerCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
  { -- | The Amazon Resource Name (ARN) of the Certificate Manager SSL\/TLS
    -- server certificate.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'serverCertificate_resourceArn' - The Amazon Resource Name (ARN) of the Certificate Manager SSL\/TLS
-- server certificate.
newServerCertificate ::
  ServerCertificate
newServerCertificate =
  ServerCertificate' {resourceArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Certificate Manager SSL\/TLS
-- server certificate.
serverCertificate_resourceArn :: Lens.Lens' ServerCertificate (Prelude.Maybe Prelude.Text)
serverCertificate_resourceArn = Lens.lens (\ServerCertificate' {resourceArn} -> resourceArn) (\s@ServerCertificate' {} a -> s {resourceArn = a} :: ServerCertificate)

instance Data.FromJSON ServerCertificate where
  parseJSON =
    Data.withObject
      "ServerCertificate"
      ( \x ->
          ServerCertificate'
            Prelude.<$> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable ServerCertificate where
  hashWithSalt _salt ServerCertificate' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ServerCertificate where
  rnf ServerCertificate' {..} = Prelude.rnf resourceArn

instance Data.ToJSON ServerCertificate where
  toJSON ServerCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ResourceArn" Data..=) Prelude.<$> resourceArn]
      )

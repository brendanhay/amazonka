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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayClientTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayClientTlsCertificate where

import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the virtual gateway\'s client\'s Transport
-- Layer Security (TLS) certificate.
--
-- /See:/ 'newVirtualGatewayClientTlsCertificate' smart constructor.
data VirtualGatewayClientTlsCertificate = VirtualGatewayClientTlsCertificate'
  { -- | A reference to an object that represents a virtual gateway\'s client\'s
    -- Secret Discovery Service certificate.
    sds :: Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate,
    -- | An object that represents a local file certificate. The certificate must
    -- meet specific requirements and you must have proxy authorization
    -- enabled. For more information, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>
    -- .
    file :: Prelude.Maybe VirtualGatewayListenerTlsFileCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayClientTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'virtualGatewayClientTlsCertificate_sds' - A reference to an object that represents a virtual gateway\'s client\'s
-- Secret Discovery Service certificate.
--
-- 'file', 'virtualGatewayClientTlsCertificate_file' - An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>
-- .
newVirtualGatewayClientTlsCertificate ::
  VirtualGatewayClientTlsCertificate
newVirtualGatewayClientTlsCertificate =
  VirtualGatewayClientTlsCertificate'
    { sds =
        Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents a virtual gateway\'s client\'s
-- Secret Discovery Service certificate.
virtualGatewayClientTlsCertificate_sds :: Lens.Lens' VirtualGatewayClientTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate)
virtualGatewayClientTlsCertificate_sds = Lens.lens (\VirtualGatewayClientTlsCertificate' {sds} -> sds) (\s@VirtualGatewayClientTlsCertificate' {} a -> s {sds = a} :: VirtualGatewayClientTlsCertificate)

-- | An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>
-- .
virtualGatewayClientTlsCertificate_file :: Lens.Lens' VirtualGatewayClientTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsFileCertificate)
virtualGatewayClientTlsCertificate_file = Lens.lens (\VirtualGatewayClientTlsCertificate' {file} -> file) (\s@VirtualGatewayClientTlsCertificate' {} a -> s {file = a} :: VirtualGatewayClientTlsCertificate)

instance
  Data.FromJSON
    VirtualGatewayClientTlsCertificate
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayClientTlsCertificate"
      ( \x ->
          VirtualGatewayClientTlsCertificate'
            Prelude.<$> (x Data..:? "sds") Prelude.<*> (x Data..:? "file")
      )

instance
  Prelude.Hashable
    VirtualGatewayClientTlsCertificate
  where
  hashWithSalt
    _salt
    VirtualGatewayClientTlsCertificate' {..} =
      _salt `Prelude.hashWithSalt` sds
        `Prelude.hashWithSalt` file

instance
  Prelude.NFData
    VirtualGatewayClientTlsCertificate
  where
  rnf VirtualGatewayClientTlsCertificate' {..} =
    Prelude.rnf sds `Prelude.seq` Prelude.rnf file

instance
  Data.ToJSON
    VirtualGatewayClientTlsCertificate
  where
  toJSON VirtualGatewayClientTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sds" Data..=) Prelude.<$> sds,
            ("file" Data..=) Prelude.<$> file
          ]
      )

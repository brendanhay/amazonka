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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate where

import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener\'s Transport Layer Security (TLS)
-- certificate.
--
-- /See:/ 'newVirtualGatewayListenerTlsCertificate' smart constructor.
data VirtualGatewayListenerTlsCertificate = VirtualGatewayListenerTlsCertificate'
  { -- | A reference to an object that represents a virtual gateway\'s
    -- listener\'s Secret Discovery Service certificate.
    sds :: Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate,
    -- | A reference to an object that represents a local file certificate.
    file :: Prelude.Maybe VirtualGatewayListenerTlsFileCertificate,
    -- | A reference to an object that represents an Certificate Manager
    -- certificate.
    acm :: Prelude.Maybe VirtualGatewayListenerTlsAcmCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'virtualGatewayListenerTlsCertificate_sds' - A reference to an object that represents a virtual gateway\'s
-- listener\'s Secret Discovery Service certificate.
--
-- 'file', 'virtualGatewayListenerTlsCertificate_file' - A reference to an object that represents a local file certificate.
--
-- 'acm', 'virtualGatewayListenerTlsCertificate_acm' - A reference to an object that represents an Certificate Manager
-- certificate.
newVirtualGatewayListenerTlsCertificate ::
  VirtualGatewayListenerTlsCertificate
newVirtualGatewayListenerTlsCertificate =
  VirtualGatewayListenerTlsCertificate'
    { sds =
        Prelude.Nothing,
      file = Prelude.Nothing,
      acm = Prelude.Nothing
    }

-- | A reference to an object that represents a virtual gateway\'s
-- listener\'s Secret Discovery Service certificate.
virtualGatewayListenerTlsCertificate_sds :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate)
virtualGatewayListenerTlsCertificate_sds = Lens.lens (\VirtualGatewayListenerTlsCertificate' {sds} -> sds) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {sds = a} :: VirtualGatewayListenerTlsCertificate)

-- | A reference to an object that represents a local file certificate.
virtualGatewayListenerTlsCertificate_file :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsFileCertificate)
virtualGatewayListenerTlsCertificate_file = Lens.lens (\VirtualGatewayListenerTlsCertificate' {file} -> file) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {file = a} :: VirtualGatewayListenerTlsCertificate)

-- | A reference to an object that represents an Certificate Manager
-- certificate.
virtualGatewayListenerTlsCertificate_acm :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsAcmCertificate)
virtualGatewayListenerTlsCertificate_acm = Lens.lens (\VirtualGatewayListenerTlsCertificate' {acm} -> acm) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {acm = a} :: VirtualGatewayListenerTlsCertificate)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsCertificate
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsCertificate"
      ( \x ->
          VirtualGatewayListenerTlsCertificate'
            Prelude.<$> (x Data..:? "sds")
            Prelude.<*> (x Data..:? "file")
            Prelude.<*> (x Data..:? "acm")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsCertificate
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsCertificate' {..} =
      _salt `Prelude.hashWithSalt` sds
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` acm

instance
  Prelude.NFData
    VirtualGatewayListenerTlsCertificate
  where
  rnf VirtualGatewayListenerTlsCertificate' {..} =
    Prelude.rnf sds
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf acm

instance
  Data.ToJSON
    VirtualGatewayListenerTlsCertificate
  where
  toJSON VirtualGatewayListenerTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sds" Data..=) Prelude.<$> sds,
            ("file" Data..=) Prelude.<$> file,
            ("acm" Data..=) Prelude.<$> acm
          ]
      )

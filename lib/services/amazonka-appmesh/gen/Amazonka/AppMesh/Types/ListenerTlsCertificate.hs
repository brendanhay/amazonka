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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsCertificate where

import Amazonka.AppMesh.Types.ListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.ListenerTlsFileCertificate
import Amazonka.AppMesh.Types.ListenerTlsSdsCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener\'s Transport Layer Security (TLS)
-- certificate.
--
-- /See:/ 'newListenerTlsCertificate' smart constructor.
data ListenerTlsCertificate = ListenerTlsCertificate'
  { -- | A reference to an object that represents a listener\'s Secret Discovery
    -- Service certificate.
    sds :: Prelude.Maybe ListenerTlsSdsCertificate,
    -- | A reference to an object that represents a local file certificate.
    file :: Prelude.Maybe ListenerTlsFileCertificate,
    -- | A reference to an object that represents an Certificate Manager
    -- certificate.
    acm :: Prelude.Maybe ListenerTlsAcmCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'listenerTlsCertificate_sds' - A reference to an object that represents a listener\'s Secret Discovery
-- Service certificate.
--
-- 'file', 'listenerTlsCertificate_file' - A reference to an object that represents a local file certificate.
--
-- 'acm', 'listenerTlsCertificate_acm' - A reference to an object that represents an Certificate Manager
-- certificate.
newListenerTlsCertificate ::
  ListenerTlsCertificate
newListenerTlsCertificate =
  ListenerTlsCertificate'
    { sds = Prelude.Nothing,
      file = Prelude.Nothing,
      acm = Prelude.Nothing
    }

-- | A reference to an object that represents a listener\'s Secret Discovery
-- Service certificate.
listenerTlsCertificate_sds :: Lens.Lens' ListenerTlsCertificate (Prelude.Maybe ListenerTlsSdsCertificate)
listenerTlsCertificate_sds = Lens.lens (\ListenerTlsCertificate' {sds} -> sds) (\s@ListenerTlsCertificate' {} a -> s {sds = a} :: ListenerTlsCertificate)

-- | A reference to an object that represents a local file certificate.
listenerTlsCertificate_file :: Lens.Lens' ListenerTlsCertificate (Prelude.Maybe ListenerTlsFileCertificate)
listenerTlsCertificate_file = Lens.lens (\ListenerTlsCertificate' {file} -> file) (\s@ListenerTlsCertificate' {} a -> s {file = a} :: ListenerTlsCertificate)

-- | A reference to an object that represents an Certificate Manager
-- certificate.
listenerTlsCertificate_acm :: Lens.Lens' ListenerTlsCertificate (Prelude.Maybe ListenerTlsAcmCertificate)
listenerTlsCertificate_acm = Lens.lens (\ListenerTlsCertificate' {acm} -> acm) (\s@ListenerTlsCertificate' {} a -> s {acm = a} :: ListenerTlsCertificate)

instance Data.FromJSON ListenerTlsCertificate where
  parseJSON =
    Data.withObject
      "ListenerTlsCertificate"
      ( \x ->
          ListenerTlsCertificate'
            Prelude.<$> (x Data..:? "sds")
            Prelude.<*> (x Data..:? "file")
            Prelude.<*> (x Data..:? "acm")
      )

instance Prelude.Hashable ListenerTlsCertificate where
  hashWithSalt _salt ListenerTlsCertificate' {..} =
    _salt `Prelude.hashWithSalt` sds
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` acm

instance Prelude.NFData ListenerTlsCertificate where
  rnf ListenerTlsCertificate' {..} =
    Prelude.rnf sds
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf acm

instance Data.ToJSON ListenerTlsCertificate where
  toJSON ListenerTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sds" Data..=) Prelude.<$> sds,
            ("file" Data..=) Prelude.<$> file,
            ("acm" Data..=) Prelude.<$> acm
          ]
      )

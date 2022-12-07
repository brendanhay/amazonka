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
-- Module      : Amazonka.AppMesh.Types.ClientTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ClientTlsCertificate where

import Amazonka.AppMesh.Types.ListenerTlsFileCertificate
import Amazonka.AppMesh.Types.ListenerTlsSdsCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the client\'s certificate.
--
-- /See:/ 'newClientTlsCertificate' smart constructor.
data ClientTlsCertificate = ClientTlsCertificate'
  { -- | A reference to an object that represents a client\'s TLS Secret
    -- Discovery Service certificate.
    sds :: Prelude.Maybe ListenerTlsSdsCertificate,
    -- | An object that represents a local file certificate. The certificate must
    -- meet specific requirements and you must have proxy authorization
    -- enabled. For more information, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>.
    file :: Prelude.Maybe ListenerTlsFileCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'clientTlsCertificate_sds' - A reference to an object that represents a client\'s TLS Secret
-- Discovery Service certificate.
--
-- 'file', 'clientTlsCertificate_file' - An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>.
newClientTlsCertificate ::
  ClientTlsCertificate
newClientTlsCertificate =
  ClientTlsCertificate'
    { sds = Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents a client\'s TLS Secret
-- Discovery Service certificate.
clientTlsCertificate_sds :: Lens.Lens' ClientTlsCertificate (Prelude.Maybe ListenerTlsSdsCertificate)
clientTlsCertificate_sds = Lens.lens (\ClientTlsCertificate' {sds} -> sds) (\s@ClientTlsCertificate' {} a -> s {sds = a} :: ClientTlsCertificate)

-- | An object that represents a local file certificate. The certificate must
-- meet specific requirements and you must have proxy authorization
-- enabled. For more information, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html Transport Layer Security (TLS)>.
clientTlsCertificate_file :: Lens.Lens' ClientTlsCertificate (Prelude.Maybe ListenerTlsFileCertificate)
clientTlsCertificate_file = Lens.lens (\ClientTlsCertificate' {file} -> file) (\s@ClientTlsCertificate' {} a -> s {file = a} :: ClientTlsCertificate)

instance Data.FromJSON ClientTlsCertificate where
  parseJSON =
    Data.withObject
      "ClientTlsCertificate"
      ( \x ->
          ClientTlsCertificate'
            Prelude.<$> (x Data..:? "sds") Prelude.<*> (x Data..:? "file")
      )

instance Prelude.Hashable ClientTlsCertificate where
  hashWithSalt _salt ClientTlsCertificate' {..} =
    _salt `Prelude.hashWithSalt` sds
      `Prelude.hashWithSalt` file

instance Prelude.NFData ClientTlsCertificate where
  rnf ClientTlsCertificate' {..} =
    Prelude.rnf sds `Prelude.seq` Prelude.rnf file

instance Data.ToJSON ClientTlsCertificate where
  toJSON ClientTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sds" Data..=) Prelude.<$> sds,
            ("file" Data..=) Prelude.<$> file
          ]
      )

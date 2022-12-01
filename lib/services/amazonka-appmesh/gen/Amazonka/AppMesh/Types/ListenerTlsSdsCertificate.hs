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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsSdsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsSdsCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the listener\'s Secret Discovery Service
-- certificate. The proxy must be configured with a local SDS provider via
-- a Unix Domain Socket. See App Mesh
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/tls.html TLS documentation>
-- for more info.
--
-- /See:/ 'newListenerTlsSdsCertificate' smart constructor.
data ListenerTlsSdsCertificate = ListenerTlsSdsCertificate'
  { -- | A reference to an object that represents the name of the secret
    -- requested from the Secret Discovery Service provider representing
    -- Transport Layer Security (TLS) materials like a certificate or
    -- certificate chain.
    secretName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsSdsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretName', 'listenerTlsSdsCertificate_secretName' - A reference to an object that represents the name of the secret
-- requested from the Secret Discovery Service provider representing
-- Transport Layer Security (TLS) materials like a certificate or
-- certificate chain.
newListenerTlsSdsCertificate ::
  -- | 'secretName'
  Prelude.Text ->
  ListenerTlsSdsCertificate
newListenerTlsSdsCertificate pSecretName_ =
  ListenerTlsSdsCertificate'
    { secretName =
        pSecretName_
    }

-- | A reference to an object that represents the name of the secret
-- requested from the Secret Discovery Service provider representing
-- Transport Layer Security (TLS) materials like a certificate or
-- certificate chain.
listenerTlsSdsCertificate_secretName :: Lens.Lens' ListenerTlsSdsCertificate Prelude.Text
listenerTlsSdsCertificate_secretName = Lens.lens (\ListenerTlsSdsCertificate' {secretName} -> secretName) (\s@ListenerTlsSdsCertificate' {} a -> s {secretName = a} :: ListenerTlsSdsCertificate)

instance Core.FromJSON ListenerTlsSdsCertificate where
  parseJSON =
    Core.withObject
      "ListenerTlsSdsCertificate"
      ( \x ->
          ListenerTlsSdsCertificate'
            Prelude.<$> (x Core..: "secretName")
      )

instance Prelude.Hashable ListenerTlsSdsCertificate where
  hashWithSalt _salt ListenerTlsSdsCertificate' {..} =
    _salt `Prelude.hashWithSalt` secretName

instance Prelude.NFData ListenerTlsSdsCertificate where
  rnf ListenerTlsSdsCertificate' {..} =
    Prelude.rnf secretName

instance Core.ToJSON ListenerTlsSdsCertificate where
  toJSON ListenerTlsSdsCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("secretName" Core..= secretName)]
      )

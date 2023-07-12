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
-- Module      : Amazonka.AppMesh.Types.TlsValidationContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TlsValidationContext where

import Amazonka.AppMesh.Types.SubjectAlternativeNames
import Amazonka.AppMesh.Types.TlsValidationContextTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents how the proxy will validate its peer during
-- Transport Layer Security (TLS) negotiation.
--
-- /See:/ 'newTlsValidationContext' smart constructor.
data TlsValidationContext = TlsValidationContext'
  { -- | A reference to an object that represents the SANs for a Transport Layer
    -- Security (TLS) validation context. If you don\'t specify SANs on the
    -- /terminating/ mesh endpoint, the Envoy proxy for that node doesn\'t
    -- verify the SAN on a peer client certificate. If you don\'t specify SANs
    -- on the /originating/ mesh endpoint, the SAN on the certificate provided
    -- by the terminating endpoint must match the mesh endpoint service
    -- discovery configuration. Since SPIRE vended certificates have a SPIFFE
    -- ID as a name, you must set the SAN since the name doesn\'t match the
    -- service discovery name.
    subjectAlternativeNames :: Prelude.Maybe SubjectAlternativeNames,
    -- | A reference to where to retrieve the trust chain when validating a
    -- peer’s Transport Layer Security (TLS) certificate.
    trust :: TlsValidationContextTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsValidationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectAlternativeNames', 'tlsValidationContext_subjectAlternativeNames' - A reference to an object that represents the SANs for a Transport Layer
-- Security (TLS) validation context. If you don\'t specify SANs on the
-- /terminating/ mesh endpoint, the Envoy proxy for that node doesn\'t
-- verify the SAN on a peer client certificate. If you don\'t specify SANs
-- on the /originating/ mesh endpoint, the SAN on the certificate provided
-- by the terminating endpoint must match the mesh endpoint service
-- discovery configuration. Since SPIRE vended certificates have a SPIFFE
-- ID as a name, you must set the SAN since the name doesn\'t match the
-- service discovery name.
--
-- 'trust', 'tlsValidationContext_trust' - A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
newTlsValidationContext ::
  -- | 'trust'
  TlsValidationContextTrust ->
  TlsValidationContext
newTlsValidationContext pTrust_ =
  TlsValidationContext'
    { subjectAlternativeNames =
        Prelude.Nothing,
      trust = pTrust_
    }

-- | A reference to an object that represents the SANs for a Transport Layer
-- Security (TLS) validation context. If you don\'t specify SANs on the
-- /terminating/ mesh endpoint, the Envoy proxy for that node doesn\'t
-- verify the SAN on a peer client certificate. If you don\'t specify SANs
-- on the /originating/ mesh endpoint, the SAN on the certificate provided
-- by the terminating endpoint must match the mesh endpoint service
-- discovery configuration. Since SPIRE vended certificates have a SPIFFE
-- ID as a name, you must set the SAN since the name doesn\'t match the
-- service discovery name.
tlsValidationContext_subjectAlternativeNames :: Lens.Lens' TlsValidationContext (Prelude.Maybe SubjectAlternativeNames)
tlsValidationContext_subjectAlternativeNames = Lens.lens (\TlsValidationContext' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@TlsValidationContext' {} a -> s {subjectAlternativeNames = a} :: TlsValidationContext)

-- | A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
tlsValidationContext_trust :: Lens.Lens' TlsValidationContext TlsValidationContextTrust
tlsValidationContext_trust = Lens.lens (\TlsValidationContext' {trust} -> trust) (\s@TlsValidationContext' {} a -> s {trust = a} :: TlsValidationContext)

instance Data.FromJSON TlsValidationContext where
  parseJSON =
    Data.withObject
      "TlsValidationContext"
      ( \x ->
          TlsValidationContext'
            Prelude.<$> (x Data..:? "subjectAlternativeNames")
            Prelude.<*> (x Data..: "trust")
      )

instance Prelude.Hashable TlsValidationContext where
  hashWithSalt _salt TlsValidationContext' {..} =
    _salt
      `Prelude.hashWithSalt` subjectAlternativeNames
      `Prelude.hashWithSalt` trust

instance Prelude.NFData TlsValidationContext where
  rnf TlsValidationContext' {..} =
    Prelude.rnf subjectAlternativeNames
      `Prelude.seq` Prelude.rnf trust

instance Data.ToJSON TlsValidationContext where
  toJSON TlsValidationContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("subjectAlternativeNames" Data..=)
              Prelude.<$> subjectAlternativeNames,
            Prelude.Just ("trust" Data..= trust)
          ]
      )

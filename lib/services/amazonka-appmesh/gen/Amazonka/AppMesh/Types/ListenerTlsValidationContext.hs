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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsValidationContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsValidationContext where

import Amazonka.AppMesh.Types.ListenerTlsValidationContextTrust
import Amazonka.AppMesh.Types.SubjectAlternativeNames
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener\'s Transport Layer Security (TLS)
-- validation context.
--
-- /See:/ 'newListenerTlsValidationContext' smart constructor.
data ListenerTlsValidationContext = ListenerTlsValidationContext'
  { -- | A reference to an object that represents the SANs for a listener\'s
    -- Transport Layer Security (TLS) validation context.
    subjectAlternativeNames :: Prelude.Maybe SubjectAlternativeNames,
    -- | A reference to where to retrieve the trust chain when validating a
    -- peer’s Transport Layer Security (TLS) certificate.
    trust :: ListenerTlsValidationContextTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsValidationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectAlternativeNames', 'listenerTlsValidationContext_subjectAlternativeNames' - A reference to an object that represents the SANs for a listener\'s
-- Transport Layer Security (TLS) validation context.
--
-- 'trust', 'listenerTlsValidationContext_trust' - A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
newListenerTlsValidationContext ::
  -- | 'trust'
  ListenerTlsValidationContextTrust ->
  ListenerTlsValidationContext
newListenerTlsValidationContext pTrust_ =
  ListenerTlsValidationContext'
    { subjectAlternativeNames =
        Prelude.Nothing,
      trust = pTrust_
    }

-- | A reference to an object that represents the SANs for a listener\'s
-- Transport Layer Security (TLS) validation context.
listenerTlsValidationContext_subjectAlternativeNames :: Lens.Lens' ListenerTlsValidationContext (Prelude.Maybe SubjectAlternativeNames)
listenerTlsValidationContext_subjectAlternativeNames = Lens.lens (\ListenerTlsValidationContext' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@ListenerTlsValidationContext' {} a -> s {subjectAlternativeNames = a} :: ListenerTlsValidationContext)

-- | A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
listenerTlsValidationContext_trust :: Lens.Lens' ListenerTlsValidationContext ListenerTlsValidationContextTrust
listenerTlsValidationContext_trust = Lens.lens (\ListenerTlsValidationContext' {trust} -> trust) (\s@ListenerTlsValidationContext' {} a -> s {trust = a} :: ListenerTlsValidationContext)

instance Core.FromJSON ListenerTlsValidationContext where
  parseJSON =
    Core.withObject
      "ListenerTlsValidationContext"
      ( \x ->
          ListenerTlsValidationContext'
            Prelude.<$> (x Core..:? "subjectAlternativeNames")
            Prelude.<*> (x Core..: "trust")
      )

instance
  Prelude.Hashable
    ListenerTlsValidationContext
  where
  hashWithSalt _salt ListenerTlsValidationContext' {..} =
    _salt
      `Prelude.hashWithSalt` subjectAlternativeNames
      `Prelude.hashWithSalt` trust

instance Prelude.NFData ListenerTlsValidationContext where
  rnf ListenerTlsValidationContext' {..} =
    Prelude.rnf subjectAlternativeNames
      `Prelude.seq` Prelude.rnf trust

instance Core.ToJSON ListenerTlsValidationContext where
  toJSON ListenerTlsValidationContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("subjectAlternativeNames" Core..=)
              Prelude.<$> subjectAlternativeNames,
            Prelude.Just ("trust" Core..= trust)
          ]
      )

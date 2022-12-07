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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContext where

import Amazonka.AppMesh.Types.SubjectAlternativeNames
import Amazonka.AppMesh.Types.VirtualGatewayTlsValidationContextTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a Transport Layer Security (TLS) validation
-- context.
--
-- /See:/ 'newVirtualGatewayTlsValidationContext' smart constructor.
data VirtualGatewayTlsValidationContext = VirtualGatewayTlsValidationContext'
  { -- | A reference to an object that represents the SANs for a virtual
    -- gateway\'s listener\'s Transport Layer Security (TLS) validation
    -- context.
    subjectAlternativeNames :: Prelude.Maybe SubjectAlternativeNames,
    -- | A reference to where to retrieve the trust chain when validating a
    -- peer’s Transport Layer Security (TLS) certificate.
    trust :: VirtualGatewayTlsValidationContextTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayTlsValidationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectAlternativeNames', 'virtualGatewayTlsValidationContext_subjectAlternativeNames' - A reference to an object that represents the SANs for a virtual
-- gateway\'s listener\'s Transport Layer Security (TLS) validation
-- context.
--
-- 'trust', 'virtualGatewayTlsValidationContext_trust' - A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
newVirtualGatewayTlsValidationContext ::
  -- | 'trust'
  VirtualGatewayTlsValidationContextTrust ->
  VirtualGatewayTlsValidationContext
newVirtualGatewayTlsValidationContext pTrust_ =
  VirtualGatewayTlsValidationContext'
    { subjectAlternativeNames =
        Prelude.Nothing,
      trust = pTrust_
    }

-- | A reference to an object that represents the SANs for a virtual
-- gateway\'s listener\'s Transport Layer Security (TLS) validation
-- context.
virtualGatewayTlsValidationContext_subjectAlternativeNames :: Lens.Lens' VirtualGatewayTlsValidationContext (Prelude.Maybe SubjectAlternativeNames)
virtualGatewayTlsValidationContext_subjectAlternativeNames = Lens.lens (\VirtualGatewayTlsValidationContext' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@VirtualGatewayTlsValidationContext' {} a -> s {subjectAlternativeNames = a} :: VirtualGatewayTlsValidationContext)

-- | A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
virtualGatewayTlsValidationContext_trust :: Lens.Lens' VirtualGatewayTlsValidationContext VirtualGatewayTlsValidationContextTrust
virtualGatewayTlsValidationContext_trust = Lens.lens (\VirtualGatewayTlsValidationContext' {trust} -> trust) (\s@VirtualGatewayTlsValidationContext' {} a -> s {trust = a} :: VirtualGatewayTlsValidationContext)

instance
  Data.FromJSON
    VirtualGatewayTlsValidationContext
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayTlsValidationContext"
      ( \x ->
          VirtualGatewayTlsValidationContext'
            Prelude.<$> (x Data..:? "subjectAlternativeNames")
            Prelude.<*> (x Data..: "trust")
      )

instance
  Prelude.Hashable
    VirtualGatewayTlsValidationContext
  where
  hashWithSalt
    _salt
    VirtualGatewayTlsValidationContext' {..} =
      _salt
        `Prelude.hashWithSalt` subjectAlternativeNames
        `Prelude.hashWithSalt` trust

instance
  Prelude.NFData
    VirtualGatewayTlsValidationContext
  where
  rnf VirtualGatewayTlsValidationContext' {..} =
    Prelude.rnf subjectAlternativeNames
      `Prelude.seq` Prelude.rnf trust

instance
  Data.ToJSON
    VirtualGatewayTlsValidationContext
  where
  toJSON VirtualGatewayTlsValidationContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("subjectAlternativeNames" Data..=)
              Prelude.<$> subjectAlternativeNames,
            Prelude.Just ("trust" Data..= trust)
          ]
      )

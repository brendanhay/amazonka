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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContext where

import Amazonka.AppMesh.Types.SubjectAlternativeNames
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsValidationContextTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual gateway\'s listener\'s Transport
-- Layer Security (TLS) validation context.
--
-- /See:/ 'newVirtualGatewayListenerTlsValidationContext' smart constructor.
data VirtualGatewayListenerTlsValidationContext = VirtualGatewayListenerTlsValidationContext'
  { -- | A reference to an object that represents the SANs for a virtual gateway
    -- listener\'s Transport Layer Security (TLS) validation context.
    subjectAlternativeNames :: Prelude.Maybe SubjectAlternativeNames,
    -- | A reference to where to retrieve the trust chain when validating a
    -- peer’s Transport Layer Security (TLS) certificate.
    trust :: VirtualGatewayListenerTlsValidationContextTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsValidationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectAlternativeNames', 'virtualGatewayListenerTlsValidationContext_subjectAlternativeNames' - A reference to an object that represents the SANs for a virtual gateway
-- listener\'s Transport Layer Security (TLS) validation context.
--
-- 'trust', 'virtualGatewayListenerTlsValidationContext_trust' - A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
newVirtualGatewayListenerTlsValidationContext ::
  -- | 'trust'
  VirtualGatewayListenerTlsValidationContextTrust ->
  VirtualGatewayListenerTlsValidationContext
newVirtualGatewayListenerTlsValidationContext pTrust_ =
  VirtualGatewayListenerTlsValidationContext'
    { subjectAlternativeNames =
        Prelude.Nothing,
      trust = pTrust_
    }

-- | A reference to an object that represents the SANs for a virtual gateway
-- listener\'s Transport Layer Security (TLS) validation context.
virtualGatewayListenerTlsValidationContext_subjectAlternativeNames :: Lens.Lens' VirtualGatewayListenerTlsValidationContext (Prelude.Maybe SubjectAlternativeNames)
virtualGatewayListenerTlsValidationContext_subjectAlternativeNames = Lens.lens (\VirtualGatewayListenerTlsValidationContext' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@VirtualGatewayListenerTlsValidationContext' {} a -> s {subjectAlternativeNames = a} :: VirtualGatewayListenerTlsValidationContext)

-- | A reference to where to retrieve the trust chain when validating a
-- peer’s Transport Layer Security (TLS) certificate.
virtualGatewayListenerTlsValidationContext_trust :: Lens.Lens' VirtualGatewayListenerTlsValidationContext VirtualGatewayListenerTlsValidationContextTrust
virtualGatewayListenerTlsValidationContext_trust = Lens.lens (\VirtualGatewayListenerTlsValidationContext' {trust} -> trust) (\s@VirtualGatewayListenerTlsValidationContext' {} a -> s {trust = a} :: VirtualGatewayListenerTlsValidationContext)

instance
  Data.FromJSON
    VirtualGatewayListenerTlsValidationContext
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayListenerTlsValidationContext"
      ( \x ->
          VirtualGatewayListenerTlsValidationContext'
            Prelude.<$> (x Data..:? "subjectAlternativeNames")
            Prelude.<*> (x Data..: "trust")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsValidationContext
  where
  hashWithSalt
    _salt
    VirtualGatewayListenerTlsValidationContext' {..} =
      _salt
        `Prelude.hashWithSalt` subjectAlternativeNames
        `Prelude.hashWithSalt` trust

instance
  Prelude.NFData
    VirtualGatewayListenerTlsValidationContext
  where
  rnf VirtualGatewayListenerTlsValidationContext' {..} =
    Prelude.rnf subjectAlternativeNames
      `Prelude.seq` Prelude.rnf trust

instance
  Data.ToJSON
    VirtualGatewayListenerTlsValidationContext
  where
  toJSON
    VirtualGatewayListenerTlsValidationContext' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("subjectAlternativeNames" Data..=)
                Prelude.<$> subjectAlternativeNames,
              Prelude.Just ("trust" Data..= trust)
            ]
        )

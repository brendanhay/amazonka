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
-- Module      : Amazonka.DirectoryService.Types.Certificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.CertificateState
import Amazonka.DirectoryService.Types.CertificateType
import Amazonka.DirectoryService.Types.ClientCertAuthSettings
import qualified Amazonka.Prelude as Prelude

-- | Information about the certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The function that the registered certificate performs. Valid values
    -- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
    -- @ClientLDAPS@.
    type' :: Prelude.Maybe CertificateType,
    -- | The date and time that the certificate was registered.
    registeredDateTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the certificate.
    state :: Prelude.Maybe CertificateState,
    -- | The identifier of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The common name for the certificate.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | Describes a state change for the certificate.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | A @ClientCertAuthSettings@ object that contains client certificate
    -- authentication settings.
    clientCertAuthSettings :: Prelude.Maybe ClientCertAuthSettings,
    -- | The date and time when the certificate will expire.
    expiryDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'certificate_type' - The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
--
-- 'registeredDateTime', 'certificate_registeredDateTime' - The date and time that the certificate was registered.
--
-- 'state', 'certificate_state' - The state of the certificate.
--
-- 'certificateId', 'certificate_certificateId' - The identifier of the certificate.
--
-- 'commonName', 'certificate_commonName' - The common name for the certificate.
--
-- 'stateReason', 'certificate_stateReason' - Describes a state change for the certificate.
--
-- 'clientCertAuthSettings', 'certificate_clientCertAuthSettings' - A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
--
-- 'expiryDateTime', 'certificate_expiryDateTime' - The date and time when the certificate will expire.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { type' = Prelude.Nothing,
      registeredDateTime = Prelude.Nothing,
      state = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      commonName = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      clientCertAuthSettings = Prelude.Nothing,
      expiryDateTime = Prelude.Nothing
    }

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
certificate_type :: Lens.Lens' Certificate (Prelude.Maybe CertificateType)
certificate_type = Lens.lens (\Certificate' {type'} -> type') (\s@Certificate' {} a -> s {type' = a} :: Certificate)

-- | The date and time that the certificate was registered.
certificate_registeredDateTime :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_registeredDateTime = Lens.lens (\Certificate' {registeredDateTime} -> registeredDateTime) (\s@Certificate' {} a -> s {registeredDateTime = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | The state of the certificate.
certificate_state :: Lens.Lens' Certificate (Prelude.Maybe CertificateState)
certificate_state = Lens.lens (\Certificate' {state} -> state) (\s@Certificate' {} a -> s {state = a} :: Certificate)

-- | The identifier of the certificate.
certificate_certificateId :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateId = Lens.lens (\Certificate' {certificateId} -> certificateId) (\s@Certificate' {} a -> s {certificateId = a} :: Certificate)

-- | The common name for the certificate.
certificate_commonName :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_commonName = Lens.lens (\Certificate' {commonName} -> commonName) (\s@Certificate' {} a -> s {commonName = a} :: Certificate)

-- | Describes a state change for the certificate.
certificate_stateReason :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_stateReason = Lens.lens (\Certificate' {stateReason} -> stateReason) (\s@Certificate' {} a -> s {stateReason = a} :: Certificate)

-- | A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
certificate_clientCertAuthSettings :: Lens.Lens' Certificate (Prelude.Maybe ClientCertAuthSettings)
certificate_clientCertAuthSettings = Lens.lens (\Certificate' {clientCertAuthSettings} -> clientCertAuthSettings) (\s@Certificate' {} a -> s {clientCertAuthSettings = a} :: Certificate)

-- | The date and time when the certificate will expire.
certificate_expiryDateTime :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_expiryDateTime = Lens.lens (\Certificate' {expiryDateTime} -> expiryDateTime) (\s@Certificate' {} a -> s {expiryDateTime = a} :: Certificate) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "RegisteredDateTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "CertificateId")
            Prelude.<*> (x Data..:? "CommonName")
            Prelude.<*> (x Data..:? "StateReason")
            Prelude.<*> (x Data..:? "ClientCertAuthSettings")
            Prelude.<*> (x Data..:? "ExpiryDateTime")
      )

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` registeredDateTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` commonName
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` clientCertAuthSettings
      `Prelude.hashWithSalt` expiryDateTime

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf registeredDateTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf commonName
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf clientCertAuthSettings
      `Prelude.seq` Prelude.rnf expiryDateTime

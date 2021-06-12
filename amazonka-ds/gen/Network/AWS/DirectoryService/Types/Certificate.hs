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
-- Module      : Network.AWS.DirectoryService.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Certificate where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.DirectoryService.Types.CertificateType
import Network.AWS.DirectoryService.Types.ClientCertAuthSettings
import qualified Network.AWS.Lens as Lens

-- | Information about the certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | A @ClientCertAuthSettings@ object that contains client certificate
    -- authentication settings.
    clientCertAuthSettings :: Core.Maybe ClientCertAuthSettings,
    -- | The date and time that the certificate was registered.
    registeredDateTime :: Core.Maybe Core.POSIX,
    -- | Describes a state change for the certificate.
    stateReason :: Core.Maybe Core.Text,
    -- | The common name for the certificate.
    commonName :: Core.Maybe Core.Text,
    -- | The state of the certificate.
    state :: Core.Maybe CertificateState,
    -- | The date and time when the certificate will expire.
    expiryDateTime :: Core.Maybe Core.POSIX,
    -- | The function that the registered certificate performs. Valid values
    -- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
    -- @ClientLDAPS@.
    type' :: Core.Maybe CertificateType,
    -- | The identifier of the certificate.
    certificateId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertAuthSettings', 'certificate_clientCertAuthSettings' - A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
--
-- 'registeredDateTime', 'certificate_registeredDateTime' - The date and time that the certificate was registered.
--
-- 'stateReason', 'certificate_stateReason' - Describes a state change for the certificate.
--
-- 'commonName', 'certificate_commonName' - The common name for the certificate.
--
-- 'state', 'certificate_state' - The state of the certificate.
--
-- 'expiryDateTime', 'certificate_expiryDateTime' - The date and time when the certificate will expire.
--
-- 'type'', 'certificate_type' - The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
--
-- 'certificateId', 'certificate_certificateId' - The identifier of the certificate.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { clientCertAuthSettings = Core.Nothing,
      registeredDateTime = Core.Nothing,
      stateReason = Core.Nothing,
      commonName = Core.Nothing,
      state = Core.Nothing,
      expiryDateTime = Core.Nothing,
      type' = Core.Nothing,
      certificateId = Core.Nothing
    }

-- | A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
certificate_clientCertAuthSettings :: Lens.Lens' Certificate (Core.Maybe ClientCertAuthSettings)
certificate_clientCertAuthSettings = Lens.lens (\Certificate' {clientCertAuthSettings} -> clientCertAuthSettings) (\s@Certificate' {} a -> s {clientCertAuthSettings = a} :: Certificate)

-- | The date and time that the certificate was registered.
certificate_registeredDateTime :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_registeredDateTime = Lens.lens (\Certificate' {registeredDateTime} -> registeredDateTime) (\s@Certificate' {} a -> s {registeredDateTime = a} :: Certificate) Core.. Lens.mapping Core._Time

-- | Describes a state change for the certificate.
certificate_stateReason :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_stateReason = Lens.lens (\Certificate' {stateReason} -> stateReason) (\s@Certificate' {} a -> s {stateReason = a} :: Certificate)

-- | The common name for the certificate.
certificate_commonName :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_commonName = Lens.lens (\Certificate' {commonName} -> commonName) (\s@Certificate' {} a -> s {commonName = a} :: Certificate)

-- | The state of the certificate.
certificate_state :: Lens.Lens' Certificate (Core.Maybe CertificateState)
certificate_state = Lens.lens (\Certificate' {state} -> state) (\s@Certificate' {} a -> s {state = a} :: Certificate)

-- | The date and time when the certificate will expire.
certificate_expiryDateTime :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_expiryDateTime = Lens.lens (\Certificate' {expiryDateTime} -> expiryDateTime) (\s@Certificate' {} a -> s {expiryDateTime = a} :: Certificate) Core.. Lens.mapping Core._Time

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
certificate_type :: Lens.Lens' Certificate (Core.Maybe CertificateType)
certificate_type = Lens.lens (\Certificate' {type'} -> type') (\s@Certificate' {} a -> s {type' = a} :: Certificate)

-- | The identifier of the certificate.
certificate_certificateId :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateId = Lens.lens (\Certificate' {certificateId} -> certificateId) (\s@Certificate' {} a -> s {certificateId = a} :: Certificate)

instance Core.FromJSON Certificate where
  parseJSON =
    Core.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Core.<$> (x Core..:? "ClientCertAuthSettings")
            Core.<*> (x Core..:? "RegisteredDateTime")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (x Core..:? "CommonName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "ExpiryDateTime")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "CertificateId")
      )

instance Core.Hashable Certificate

instance Core.NFData Certificate

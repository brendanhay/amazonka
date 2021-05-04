{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.DirectoryService.Types.CertificateType
import Network.AWS.DirectoryService.Types.ClientCertAuthSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | A @ClientCertAuthSettings@ object that contains client certificate
    -- authentication settings.
    clientCertAuthSettings :: Prelude.Maybe ClientCertAuthSettings,
    -- | The date and time that the certificate was registered.
    registeredDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Describes a state change for the certificate.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The common name for the certificate.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | The state of the certificate.
    state :: Prelude.Maybe CertificateState,
    -- | The date and time when the certificate will expire.
    expiryDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The function that the registered certificate performs. Valid values
    -- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
    -- @ClientLDAPS@.
    type' :: Prelude.Maybe CertificateType,
    -- | The identifier of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { clientCertAuthSettings =
        Prelude.Nothing,
      registeredDateTime = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      commonName = Prelude.Nothing,
      state = Prelude.Nothing,
      expiryDateTime = Prelude.Nothing,
      type' = Prelude.Nothing,
      certificateId = Prelude.Nothing
    }

-- | A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
certificate_clientCertAuthSettings :: Lens.Lens' Certificate (Prelude.Maybe ClientCertAuthSettings)
certificate_clientCertAuthSettings = Lens.lens (\Certificate' {clientCertAuthSettings} -> clientCertAuthSettings) (\s@Certificate' {} a -> s {clientCertAuthSettings = a} :: Certificate)

-- | The date and time that the certificate was registered.
certificate_registeredDateTime :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_registeredDateTime = Lens.lens (\Certificate' {registeredDateTime} -> registeredDateTime) (\s@Certificate' {} a -> s {registeredDateTime = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | Describes a state change for the certificate.
certificate_stateReason :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_stateReason = Lens.lens (\Certificate' {stateReason} -> stateReason) (\s@Certificate' {} a -> s {stateReason = a} :: Certificate)

-- | The common name for the certificate.
certificate_commonName :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_commonName = Lens.lens (\Certificate' {commonName} -> commonName) (\s@Certificate' {} a -> s {commonName = a} :: Certificate)

-- | The state of the certificate.
certificate_state :: Lens.Lens' Certificate (Prelude.Maybe CertificateState)
certificate_state = Lens.lens (\Certificate' {state} -> state) (\s@Certificate' {} a -> s {state = a} :: Certificate)

-- | The date and time when the certificate will expire.
certificate_expiryDateTime :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_expiryDateTime = Lens.lens (\Certificate' {expiryDateTime} -> expiryDateTime) (\s@Certificate' {} a -> s {expiryDateTime = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
certificate_type :: Lens.Lens' Certificate (Prelude.Maybe CertificateType)
certificate_type = Lens.lens (\Certificate' {type'} -> type') (\s@Certificate' {} a -> s {type' = a} :: Certificate)

-- | The identifier of the certificate.
certificate_certificateId :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateId = Lens.lens (\Certificate' {certificateId} -> certificateId) (\s@Certificate' {} a -> s {certificateId = a} :: Certificate)

instance Prelude.FromJSON Certificate where
  parseJSON =
    Prelude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Prelude..:? "ClientCertAuthSettings")
            Prelude.<*> (x Prelude..:? "RegisteredDateTime")
            Prelude.<*> (x Prelude..:? "StateReason")
            Prelude.<*> (x Prelude..:? "CommonName")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "ExpiryDateTime")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "CertificateId")
      )

instance Prelude.Hashable Certificate

instance Prelude.NFData Certificate

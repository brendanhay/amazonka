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
-- Module      : Network.AWS.DirectoryService.Types.CertificateInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.CertificateInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.DirectoryService.Types.CertificateType
import qualified Network.AWS.Lens as Lens

-- | Contains general information about a certificate.
--
-- /See:/ 'newCertificateInfo' smart constructor.
data CertificateInfo = CertificateInfo'
  { -- | The common name for the certificate.
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
-- Create a value of 'CertificateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commonName', 'certificateInfo_commonName' - The common name for the certificate.
--
-- 'state', 'certificateInfo_state' - The state of the certificate.
--
-- 'expiryDateTime', 'certificateInfo_expiryDateTime' - The date and time when the certificate will expire.
--
-- 'type'', 'certificateInfo_type' - The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
--
-- 'certificateId', 'certificateInfo_certificateId' - The identifier of the certificate.
newCertificateInfo ::
  CertificateInfo
newCertificateInfo =
  CertificateInfo'
    { commonName = Core.Nothing,
      state = Core.Nothing,
      expiryDateTime = Core.Nothing,
      type' = Core.Nothing,
      certificateId = Core.Nothing
    }

-- | The common name for the certificate.
certificateInfo_commonName :: Lens.Lens' CertificateInfo (Core.Maybe Core.Text)
certificateInfo_commonName = Lens.lens (\CertificateInfo' {commonName} -> commonName) (\s@CertificateInfo' {} a -> s {commonName = a} :: CertificateInfo)

-- | The state of the certificate.
certificateInfo_state :: Lens.Lens' CertificateInfo (Core.Maybe CertificateState)
certificateInfo_state = Lens.lens (\CertificateInfo' {state} -> state) (\s@CertificateInfo' {} a -> s {state = a} :: CertificateInfo)

-- | The date and time when the certificate will expire.
certificateInfo_expiryDateTime :: Lens.Lens' CertificateInfo (Core.Maybe Core.UTCTime)
certificateInfo_expiryDateTime = Lens.lens (\CertificateInfo' {expiryDateTime} -> expiryDateTime) (\s@CertificateInfo' {} a -> s {expiryDateTime = a} :: CertificateInfo) Core.. Lens.mapping Core._Time

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
certificateInfo_type :: Lens.Lens' CertificateInfo (Core.Maybe CertificateType)
certificateInfo_type = Lens.lens (\CertificateInfo' {type'} -> type') (\s@CertificateInfo' {} a -> s {type' = a} :: CertificateInfo)

-- | The identifier of the certificate.
certificateInfo_certificateId :: Lens.Lens' CertificateInfo (Core.Maybe Core.Text)
certificateInfo_certificateId = Lens.lens (\CertificateInfo' {certificateId} -> certificateId) (\s@CertificateInfo' {} a -> s {certificateId = a} :: CertificateInfo)

instance Core.FromJSON CertificateInfo where
  parseJSON =
    Core.withObject
      "CertificateInfo"
      ( \x ->
          CertificateInfo'
            Core.<$> (x Core..:? "CommonName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "ExpiryDateTime")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "CertificateId")
      )

instance Core.Hashable CertificateInfo

instance Core.NFData CertificateInfo

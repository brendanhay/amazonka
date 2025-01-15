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
-- Module      : Amazonka.DirectoryService.Types.CertificateInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.CertificateInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.CertificateState
import Amazonka.DirectoryService.Types.CertificateType
import qualified Amazonka.Prelude as Prelude

-- | Contains general information about a certificate.
--
-- /See:/ 'newCertificateInfo' smart constructor.
data CertificateInfo = CertificateInfo'
  { -- | The identifier of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The common name for the certificate.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the certificate will expire.
    expiryDateTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the certificate.
    state :: Prelude.Maybe CertificateState,
    -- | The function that the registered certificate performs. Valid values
    -- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
    -- @ClientLDAPS@.
    type' :: Prelude.Maybe CertificateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'certificateInfo_certificateId' - The identifier of the certificate.
--
-- 'commonName', 'certificateInfo_commonName' - The common name for the certificate.
--
-- 'expiryDateTime', 'certificateInfo_expiryDateTime' - The date and time when the certificate will expire.
--
-- 'state', 'certificateInfo_state' - The state of the certificate.
--
-- 'type'', 'certificateInfo_type' - The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
newCertificateInfo ::
  CertificateInfo
newCertificateInfo =
  CertificateInfo'
    { certificateId = Prelude.Nothing,
      commonName = Prelude.Nothing,
      expiryDateTime = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The identifier of the certificate.
certificateInfo_certificateId :: Lens.Lens' CertificateInfo (Prelude.Maybe Prelude.Text)
certificateInfo_certificateId = Lens.lens (\CertificateInfo' {certificateId} -> certificateId) (\s@CertificateInfo' {} a -> s {certificateId = a} :: CertificateInfo)

-- | The common name for the certificate.
certificateInfo_commonName :: Lens.Lens' CertificateInfo (Prelude.Maybe Prelude.Text)
certificateInfo_commonName = Lens.lens (\CertificateInfo' {commonName} -> commonName) (\s@CertificateInfo' {} a -> s {commonName = a} :: CertificateInfo)

-- | The date and time when the certificate will expire.
certificateInfo_expiryDateTime :: Lens.Lens' CertificateInfo (Prelude.Maybe Prelude.UTCTime)
certificateInfo_expiryDateTime = Lens.lens (\CertificateInfo' {expiryDateTime} -> expiryDateTime) (\s@CertificateInfo' {} a -> s {expiryDateTime = a} :: CertificateInfo) Prelude.. Lens.mapping Data._Time

-- | The state of the certificate.
certificateInfo_state :: Lens.Lens' CertificateInfo (Prelude.Maybe CertificateState)
certificateInfo_state = Lens.lens (\CertificateInfo' {state} -> state) (\s@CertificateInfo' {} a -> s {state = a} :: CertificateInfo)

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
certificateInfo_type :: Lens.Lens' CertificateInfo (Prelude.Maybe CertificateType)
certificateInfo_type = Lens.lens (\CertificateInfo' {type'} -> type') (\s@CertificateInfo' {} a -> s {type' = a} :: CertificateInfo)

instance Data.FromJSON CertificateInfo where
  parseJSON =
    Data.withObject
      "CertificateInfo"
      ( \x ->
          CertificateInfo'
            Prelude.<$> (x Data..:? "CertificateId")
            Prelude.<*> (x Data..:? "CommonName")
            Prelude.<*> (x Data..:? "ExpiryDateTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable CertificateInfo where
  hashWithSalt _salt CertificateInfo' {..} =
    _salt
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` commonName
      `Prelude.hashWithSalt` expiryDateTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CertificateInfo where
  rnf CertificateInfo' {..} =
    Prelude.rnf certificateId `Prelude.seq`
      Prelude.rnf commonName `Prelude.seq`
        Prelude.rnf expiryDateTime `Prelude.seq`
          Prelude.rnf state `Prelude.seq`
            Prelude.rnf type'

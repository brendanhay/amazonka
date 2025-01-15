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
-- Module      : Amazonka.IoT.Types.CertificateValidity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CertificateValidity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When the certificate is valid.
--
-- /See:/ 'newCertificateValidity' smart constructor.
data CertificateValidity = CertificateValidity'
  { -- | The certificate is not valid after this date.
    notAfter :: Prelude.Maybe Data.POSIX,
    -- | The certificate is not valid before this date.
    notBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateValidity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notAfter', 'certificateValidity_notAfter' - The certificate is not valid after this date.
--
-- 'notBefore', 'certificateValidity_notBefore' - The certificate is not valid before this date.
newCertificateValidity ::
  CertificateValidity
newCertificateValidity =
  CertificateValidity'
    { notAfter = Prelude.Nothing,
      notBefore = Prelude.Nothing
    }

-- | The certificate is not valid after this date.
certificateValidity_notAfter :: Lens.Lens' CertificateValidity (Prelude.Maybe Prelude.UTCTime)
certificateValidity_notAfter = Lens.lens (\CertificateValidity' {notAfter} -> notAfter) (\s@CertificateValidity' {} a -> s {notAfter = a} :: CertificateValidity) Prelude.. Lens.mapping Data._Time

-- | The certificate is not valid before this date.
certificateValidity_notBefore :: Lens.Lens' CertificateValidity (Prelude.Maybe Prelude.UTCTime)
certificateValidity_notBefore = Lens.lens (\CertificateValidity' {notBefore} -> notBefore) (\s@CertificateValidity' {} a -> s {notBefore = a} :: CertificateValidity) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CertificateValidity where
  parseJSON =
    Data.withObject
      "CertificateValidity"
      ( \x ->
          CertificateValidity'
            Prelude.<$> (x Data..:? "notAfter")
            Prelude.<*> (x Data..:? "notBefore")
      )

instance Prelude.Hashable CertificateValidity where
  hashWithSalt _salt CertificateValidity' {..} =
    _salt
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` notBefore

instance Prelude.NFData CertificateValidity where
  rnf CertificateValidity' {..} =
    Prelude.rnf notAfter `Prelude.seq`
      Prelude.rnf notBefore

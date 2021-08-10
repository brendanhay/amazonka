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
-- Module      : Network.AWS.IoT.Types.CertificateValidity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateValidity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When the certificate is valid.
--
-- /See:/ 'newCertificateValidity' smart constructor.
data CertificateValidity = CertificateValidity'
  { -- | The certificate is not valid before this date.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The certificate is not valid after this date.
    notAfter :: Prelude.Maybe Core.POSIX
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
-- 'notBefore', 'certificateValidity_notBefore' - The certificate is not valid before this date.
--
-- 'notAfter', 'certificateValidity_notAfter' - The certificate is not valid after this date.
newCertificateValidity ::
  CertificateValidity
newCertificateValidity =
  CertificateValidity'
    { notBefore = Prelude.Nothing,
      notAfter = Prelude.Nothing
    }

-- | The certificate is not valid before this date.
certificateValidity_notBefore :: Lens.Lens' CertificateValidity (Prelude.Maybe Prelude.UTCTime)
certificateValidity_notBefore = Lens.lens (\CertificateValidity' {notBefore} -> notBefore) (\s@CertificateValidity' {} a -> s {notBefore = a} :: CertificateValidity) Prelude.. Lens.mapping Core._Time

-- | The certificate is not valid after this date.
certificateValidity_notAfter :: Lens.Lens' CertificateValidity (Prelude.Maybe Prelude.UTCTime)
certificateValidity_notAfter = Lens.lens (\CertificateValidity' {notAfter} -> notAfter) (\s@CertificateValidity' {} a -> s {notAfter = a} :: CertificateValidity) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CertificateValidity where
  parseJSON =
    Core.withObject
      "CertificateValidity"
      ( \x ->
          CertificateValidity'
            Prelude.<$> (x Core..:? "notBefore")
            Prelude.<*> (x Core..:? "notAfter")
      )

instance Prelude.Hashable CertificateValidity

instance Prelude.NFData CertificateValidity

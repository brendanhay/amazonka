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
-- Module      : Network.AWS.IoTWireless.Types.CertificateList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Types.CertificateList where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types.SigningAlg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of sidewalk certificates.
--
-- /See:/ 'newCertificateList' smart constructor.
data CertificateList = CertificateList'
  { -- | The certificate chain algorithm provided by sidewalk.
    signingAlg :: SigningAlg,
    -- | The value of the chosen sidewalk certificate.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingAlg', 'certificateList_signingAlg' - The certificate chain algorithm provided by sidewalk.
--
-- 'value', 'certificateList_value' - The value of the chosen sidewalk certificate.
newCertificateList ::
  -- | 'signingAlg'
  SigningAlg ->
  -- | 'value'
  Prelude.Text ->
  CertificateList
newCertificateList pSigningAlg_ pValue_ =
  CertificateList'
    { signingAlg = pSigningAlg_,
      value = pValue_
    }

-- | The certificate chain algorithm provided by sidewalk.
certificateList_signingAlg :: Lens.Lens' CertificateList SigningAlg
certificateList_signingAlg = Lens.lens (\CertificateList' {signingAlg} -> signingAlg) (\s@CertificateList' {} a -> s {signingAlg = a} :: CertificateList)

-- | The value of the chosen sidewalk certificate.
certificateList_value :: Lens.Lens' CertificateList Prelude.Text
certificateList_value = Lens.lens (\CertificateList' {value} -> value) (\s@CertificateList' {} a -> s {value = a} :: CertificateList)

instance Core.FromJSON CertificateList where
  parseJSON =
    Core.withObject
      "CertificateList"
      ( \x ->
          CertificateList'
            Prelude.<$> (x Core..: "SigningAlg")
            Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable CertificateList

instance Prelude.NFData CertificateList

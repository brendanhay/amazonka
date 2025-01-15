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
-- Module      : Amazonka.IoTWireless.Types.CertificateList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.CertificateList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.SigningAlg
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON CertificateList where
  parseJSON =
    Data.withObject
      "CertificateList"
      ( \x ->
          CertificateList'
            Prelude.<$> (x Data..: "SigningAlg")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable CertificateList where
  hashWithSalt _salt CertificateList' {..} =
    _salt
      `Prelude.hashWithSalt` signingAlg
      `Prelude.hashWithSalt` value

instance Prelude.NFData CertificateList where
  rnf CertificateList' {..} =
    Prelude.rnf signingAlg `Prelude.seq`
      Prelude.rnf value

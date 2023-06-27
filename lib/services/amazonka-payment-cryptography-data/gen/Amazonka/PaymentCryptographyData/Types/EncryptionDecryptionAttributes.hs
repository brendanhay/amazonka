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
-- Module      : Amazonka.PaymentCryptographyData.Types.EncryptionDecryptionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.EncryptionDecryptionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.AsymmetricEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.DukptEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.SymmetricEncryptionAttributes
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to perform encryption and decryption
-- operations.
--
-- /See:/ 'newEncryptionDecryptionAttributes' smart constructor.
data EncryptionDecryptionAttributes = EncryptionDecryptionAttributes'
  { asymmetric :: Prelude.Maybe AsymmetricEncryptionAttributes,
    dukpt :: Prelude.Maybe DukptEncryptionAttributes,
    -- | Parameters that are required to perform encryption and decryption using
    -- symmetric keys.
    symmetric :: Prelude.Maybe SymmetricEncryptionAttributes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionDecryptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asymmetric', 'encryptionDecryptionAttributes_asymmetric' - Undocumented member.
--
-- 'dukpt', 'encryptionDecryptionAttributes_dukpt' - Undocumented member.
--
-- 'symmetric', 'encryptionDecryptionAttributes_symmetric' - Parameters that are required to perform encryption and decryption using
-- symmetric keys.
newEncryptionDecryptionAttributes ::
  EncryptionDecryptionAttributes
newEncryptionDecryptionAttributes =
  EncryptionDecryptionAttributes'
    { asymmetric =
        Prelude.Nothing,
      dukpt = Prelude.Nothing,
      symmetric = Prelude.Nothing
    }

-- | Undocumented member.
encryptionDecryptionAttributes_asymmetric :: Lens.Lens' EncryptionDecryptionAttributes (Prelude.Maybe AsymmetricEncryptionAttributes)
encryptionDecryptionAttributes_asymmetric = Lens.lens (\EncryptionDecryptionAttributes' {asymmetric} -> asymmetric) (\s@EncryptionDecryptionAttributes' {} a -> s {asymmetric = a} :: EncryptionDecryptionAttributes)

-- | Undocumented member.
encryptionDecryptionAttributes_dukpt :: Lens.Lens' EncryptionDecryptionAttributes (Prelude.Maybe DukptEncryptionAttributes)
encryptionDecryptionAttributes_dukpt = Lens.lens (\EncryptionDecryptionAttributes' {dukpt} -> dukpt) (\s@EncryptionDecryptionAttributes' {} a -> s {dukpt = a} :: EncryptionDecryptionAttributes)

-- | Parameters that are required to perform encryption and decryption using
-- symmetric keys.
encryptionDecryptionAttributes_symmetric :: Lens.Lens' EncryptionDecryptionAttributes (Prelude.Maybe SymmetricEncryptionAttributes)
encryptionDecryptionAttributes_symmetric = Lens.lens (\EncryptionDecryptionAttributes' {symmetric} -> symmetric) (\s@EncryptionDecryptionAttributes' {} a -> s {symmetric = a} :: EncryptionDecryptionAttributes)

instance
  Prelude.Hashable
    EncryptionDecryptionAttributes
  where
  hashWithSalt
    _salt
    EncryptionDecryptionAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` asymmetric
        `Prelude.hashWithSalt` dukpt
        `Prelude.hashWithSalt` symmetric

instance
  Prelude.NFData
    EncryptionDecryptionAttributes
  where
  rnf EncryptionDecryptionAttributes' {..} =
    Prelude.rnf asymmetric
      `Prelude.seq` Prelude.rnf dukpt
      `Prelude.seq` Prelude.rnf symmetric

instance Data.ToJSON EncryptionDecryptionAttributes where
  toJSON EncryptionDecryptionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Asymmetric" Data..=) Prelude.<$> asymmetric,
            ("Dukpt" Data..=) Prelude.<$> dukpt,
            ("Symmetric" Data..=) Prelude.<$> symmetric
          ]
      )

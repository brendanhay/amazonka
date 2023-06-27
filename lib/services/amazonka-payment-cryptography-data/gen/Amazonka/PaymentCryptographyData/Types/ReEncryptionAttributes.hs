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
-- Module      : Amazonka.PaymentCryptographyData.Types.ReEncryptionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.ReEncryptionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.DukptEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.SymmetricEncryptionAttributes
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to perform reencryption operation.
--
-- /See:/ 'newReEncryptionAttributes' smart constructor.
data ReEncryptionAttributes = ReEncryptionAttributes'
  { dukpt :: Prelude.Maybe DukptEncryptionAttributes,
    -- | Parameters that are required to encrypt data using symmetric keys.
    symmetric :: Prelude.Maybe SymmetricEncryptionAttributes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReEncryptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukpt', 'reEncryptionAttributes_dukpt' - Undocumented member.
--
-- 'symmetric', 'reEncryptionAttributes_symmetric' - Parameters that are required to encrypt data using symmetric keys.
newReEncryptionAttributes ::
  ReEncryptionAttributes
newReEncryptionAttributes =
  ReEncryptionAttributes'
    { dukpt = Prelude.Nothing,
      symmetric = Prelude.Nothing
    }

-- | Undocumented member.
reEncryptionAttributes_dukpt :: Lens.Lens' ReEncryptionAttributes (Prelude.Maybe DukptEncryptionAttributes)
reEncryptionAttributes_dukpt = Lens.lens (\ReEncryptionAttributes' {dukpt} -> dukpt) (\s@ReEncryptionAttributes' {} a -> s {dukpt = a} :: ReEncryptionAttributes)

-- | Parameters that are required to encrypt data using symmetric keys.
reEncryptionAttributes_symmetric :: Lens.Lens' ReEncryptionAttributes (Prelude.Maybe SymmetricEncryptionAttributes)
reEncryptionAttributes_symmetric = Lens.lens (\ReEncryptionAttributes' {symmetric} -> symmetric) (\s@ReEncryptionAttributes' {} a -> s {symmetric = a} :: ReEncryptionAttributes)

instance Prelude.Hashable ReEncryptionAttributes where
  hashWithSalt _salt ReEncryptionAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dukpt
      `Prelude.hashWithSalt` symmetric

instance Prelude.NFData ReEncryptionAttributes where
  rnf ReEncryptionAttributes' {..} =
    Prelude.rnf dukpt
      `Prelude.seq` Prelude.rnf symmetric

instance Data.ToJSON ReEncryptionAttributes where
  toJSON ReEncryptionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dukpt" Data..=) Prelude.<$> dukpt,
            ("Symmetric" Data..=) Prelude.<$> symmetric
          ]
      )

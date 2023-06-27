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
-- Module      : Amazonka.PaymentCryptographyData.Types.DukptAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DukptAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.DukptDerivationType
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are used for Derived Unique Key Per Transaction (DUKPT)
-- derivation algorithm.
--
-- /See:/ 'newDukptAttributes' smart constructor.
data DukptAttributes = DukptAttributes'
  { -- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
    -- Key Serial Number (KSN). This must be less than or equal to the strength
    -- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
    -- for a BDK of @AES_128@ or @TDES_2KEY@.
    dukptDerivationType :: DukptDerivationType,
    -- | The unique identifier known as Key Serial Number (KSN) that comes from
    -- an encrypting device using DUKPT encryption method. The KSN is derived
    -- from the encrypting device unique identifier and an internal transaction
    -- counter.
    keySerialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DukptAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukptDerivationType', 'dukptAttributes_dukptDerivationType' - The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@.
--
-- 'keySerialNumber', 'dukptAttributes_keySerialNumber' - The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
newDukptAttributes ::
  -- | 'dukptDerivationType'
  DukptDerivationType ->
  -- | 'keySerialNumber'
  Prelude.Text ->
  DukptAttributes
newDukptAttributes
  pDukptDerivationType_
  pKeySerialNumber_ =
    DukptAttributes'
      { dukptDerivationType =
          pDukptDerivationType_,
        keySerialNumber = pKeySerialNumber_
      }

-- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@.
dukptAttributes_dukptDerivationType :: Lens.Lens' DukptAttributes DukptDerivationType
dukptAttributes_dukptDerivationType = Lens.lens (\DukptAttributes' {dukptDerivationType} -> dukptDerivationType) (\s@DukptAttributes' {} a -> s {dukptDerivationType = a} :: DukptAttributes)

-- | The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
dukptAttributes_keySerialNumber :: Lens.Lens' DukptAttributes Prelude.Text
dukptAttributes_keySerialNumber = Lens.lens (\DukptAttributes' {keySerialNumber} -> keySerialNumber) (\s@DukptAttributes' {} a -> s {keySerialNumber = a} :: DukptAttributes)

instance Prelude.Hashable DukptAttributes where
  hashWithSalt _salt DukptAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dukptDerivationType
      `Prelude.hashWithSalt` keySerialNumber

instance Prelude.NFData DukptAttributes where
  rnf DukptAttributes' {..} =
    Prelude.rnf dukptDerivationType
      `Prelude.seq` Prelude.rnf keySerialNumber

instance Data.ToJSON DukptAttributes where
  toJSON DukptAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DukptDerivationType" Data..= dukptDerivationType),
            Prelude.Just
              ("KeySerialNumber" Data..= keySerialNumber)
          ]
      )

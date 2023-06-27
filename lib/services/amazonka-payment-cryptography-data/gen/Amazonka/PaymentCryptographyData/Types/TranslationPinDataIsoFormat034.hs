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
-- Module      : Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat034
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat034 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for tranlation between ISO9564 PIN format
-- 0,3,4 tranlation.
--
-- /See:/ 'newTranslationPinDataIsoFormat034' smart constructor.
data TranslationPinDataIsoFormat034 = TranslationPinDataIsoFormat034'
  { -- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
    -- identifier for a payment credit or debit card and associates the card to
    -- a specific account holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslationPinDataIsoFormat034' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryAccountNumber', 'translationPinDataIsoFormat034_primaryAccountNumber' - The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
newTranslationPinDataIsoFormat034 ::
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  TranslationPinDataIsoFormat034
newTranslationPinDataIsoFormat034
  pPrimaryAccountNumber_ =
    TranslationPinDataIsoFormat034'
      { primaryAccountNumber =
          Data._Sensitive
            Lens.# pPrimaryAccountNumber_
      }

-- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
translationPinDataIsoFormat034_primaryAccountNumber :: Lens.Lens' TranslationPinDataIsoFormat034 Prelude.Text
translationPinDataIsoFormat034_primaryAccountNumber = Lens.lens (\TranslationPinDataIsoFormat034' {primaryAccountNumber} -> primaryAccountNumber) (\s@TranslationPinDataIsoFormat034' {} a -> s {primaryAccountNumber = a} :: TranslationPinDataIsoFormat034) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    TranslationPinDataIsoFormat034
  where
  hashWithSalt
    _salt
    TranslationPinDataIsoFormat034' {..} =
      _salt `Prelude.hashWithSalt` primaryAccountNumber

instance
  Prelude.NFData
    TranslationPinDataIsoFormat034
  where
  rnf TranslationPinDataIsoFormat034' {..} =
    Prelude.rnf primaryAccountNumber

instance Data.ToJSON TranslationPinDataIsoFormat034 where
  toJSON TranslationPinDataIsoFormat034' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              )
          ]
      )

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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyVisa
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyVisa where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive session key for Visa payment card for ARQC
-- verification.
--
-- /See:/ 'newSessionKeyVisa' smart constructor.
data SessionKeyVisa = SessionKeyVisa'
  { -- | A number that identifies and differentiates payment cards with the same
    -- Primary Account Number (PAN).
    panSequenceNumber :: Prelude.Text,
    -- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
    -- identifier for a payment credit or debit card and associates the card to
    -- a specific account holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeyVisa' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'panSequenceNumber', 'sessionKeyVisa_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'primaryAccountNumber', 'sessionKeyVisa_primaryAccountNumber' - The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
newSessionKeyVisa ::
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  SessionKeyVisa
newSessionKeyVisa
  pPanSequenceNumber_
  pPrimaryAccountNumber_ =
    SessionKeyVisa'
      { panSequenceNumber =
          pPanSequenceNumber_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
sessionKeyVisa_panSequenceNumber :: Lens.Lens' SessionKeyVisa Prelude.Text
sessionKeyVisa_panSequenceNumber = Lens.lens (\SessionKeyVisa' {panSequenceNumber} -> panSequenceNumber) (\s@SessionKeyVisa' {} a -> s {panSequenceNumber = a} :: SessionKeyVisa)

-- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
sessionKeyVisa_primaryAccountNumber :: Lens.Lens' SessionKeyVisa Prelude.Text
sessionKeyVisa_primaryAccountNumber = Lens.lens (\SessionKeyVisa' {primaryAccountNumber} -> primaryAccountNumber) (\s@SessionKeyVisa' {} a -> s {primaryAccountNumber = a} :: SessionKeyVisa) Prelude.. Data._Sensitive

instance Prelude.Hashable SessionKeyVisa where
  hashWithSalt _salt SessionKeyVisa' {..} =
    _salt
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData SessionKeyVisa where
  rnf SessionKeyVisa' {..} =
    Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToJSON SessionKeyVisa where
  toJSON SessionKeyVisa' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PanSequenceNumber" Data..= panSequenceNumber),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              )
          ]
      )

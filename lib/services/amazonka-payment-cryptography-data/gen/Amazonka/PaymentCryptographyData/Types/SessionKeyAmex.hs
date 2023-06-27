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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyAmex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyAmex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive session key for an Amex payment card.
--
-- /See:/ 'newSessionKeyAmex' smart constructor.
data SessionKeyAmex = SessionKeyAmex'
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
-- Create a value of 'SessionKeyAmex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'panSequenceNumber', 'sessionKeyAmex_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'primaryAccountNumber', 'sessionKeyAmex_primaryAccountNumber' - The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
newSessionKeyAmex ::
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  SessionKeyAmex
newSessionKeyAmex
  pPanSequenceNumber_
  pPrimaryAccountNumber_ =
    SessionKeyAmex'
      { panSequenceNumber =
          pPanSequenceNumber_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
sessionKeyAmex_panSequenceNumber :: Lens.Lens' SessionKeyAmex Prelude.Text
sessionKeyAmex_panSequenceNumber = Lens.lens (\SessionKeyAmex' {panSequenceNumber} -> panSequenceNumber) (\s@SessionKeyAmex' {} a -> s {panSequenceNumber = a} :: SessionKeyAmex)

-- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
sessionKeyAmex_primaryAccountNumber :: Lens.Lens' SessionKeyAmex Prelude.Text
sessionKeyAmex_primaryAccountNumber = Lens.lens (\SessionKeyAmex' {primaryAccountNumber} -> primaryAccountNumber) (\s@SessionKeyAmex' {} a -> s {primaryAccountNumber = a} :: SessionKeyAmex) Prelude.. Data._Sensitive

instance Prelude.Hashable SessionKeyAmex where
  hashWithSalt _salt SessionKeyAmex' {..} =
    _salt
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData SessionKeyAmex where
  rnf SessionKeyAmex' {..} =
    Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToJSON SessionKeyAmex where
  toJSON SessionKeyAmex' {..} =
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

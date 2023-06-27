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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyEmv2000
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyEmv2000 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive session key for an Emv2000 payment card for ARQC
-- verification.
--
-- /See:/ 'newSessionKeyEmv2000' smart constructor.
data SessionKeyEmv2000 = SessionKeyEmv2000'
  { -- | The transaction counter that is provided by the terminal during
    -- transaction processing.
    applicationTransactionCounter :: Prelude.Text,
    -- | A number that identifies and differentiates payment cards with the same
    -- Primary Account Number (PAN).
    panSequenceNumber :: Prelude.Text,
    -- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
    -- identifier for a payment credit or debit card and associates the card to
    -- a specific account holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeyEmv2000' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationTransactionCounter', 'sessionKeyEmv2000_applicationTransactionCounter' - The transaction counter that is provided by the terminal during
-- transaction processing.
--
-- 'panSequenceNumber', 'sessionKeyEmv2000_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'primaryAccountNumber', 'sessionKeyEmv2000_primaryAccountNumber' - The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
newSessionKeyEmv2000 ::
  -- | 'applicationTransactionCounter'
  Prelude.Text ->
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  SessionKeyEmv2000
newSessionKeyEmv2000
  pApplicationTransactionCounter_
  pPanSequenceNumber_
  pPrimaryAccountNumber_ =
    SessionKeyEmv2000'
      { applicationTransactionCounter =
          pApplicationTransactionCounter_,
        panSequenceNumber = pPanSequenceNumber_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | The transaction counter that is provided by the terminal during
-- transaction processing.
sessionKeyEmv2000_applicationTransactionCounter :: Lens.Lens' SessionKeyEmv2000 Prelude.Text
sessionKeyEmv2000_applicationTransactionCounter = Lens.lens (\SessionKeyEmv2000' {applicationTransactionCounter} -> applicationTransactionCounter) (\s@SessionKeyEmv2000' {} a -> s {applicationTransactionCounter = a} :: SessionKeyEmv2000)

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
sessionKeyEmv2000_panSequenceNumber :: Lens.Lens' SessionKeyEmv2000 Prelude.Text
sessionKeyEmv2000_panSequenceNumber = Lens.lens (\SessionKeyEmv2000' {panSequenceNumber} -> panSequenceNumber) (\s@SessionKeyEmv2000' {} a -> s {panSequenceNumber = a} :: SessionKeyEmv2000)

-- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
sessionKeyEmv2000_primaryAccountNumber :: Lens.Lens' SessionKeyEmv2000 Prelude.Text
sessionKeyEmv2000_primaryAccountNumber = Lens.lens (\SessionKeyEmv2000' {primaryAccountNumber} -> primaryAccountNumber) (\s@SessionKeyEmv2000' {} a -> s {primaryAccountNumber = a} :: SessionKeyEmv2000) Prelude.. Data._Sensitive

instance Prelude.Hashable SessionKeyEmv2000 where
  hashWithSalt _salt SessionKeyEmv2000' {..} =
    _salt
      `Prelude.hashWithSalt` applicationTransactionCounter
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData SessionKeyEmv2000 where
  rnf SessionKeyEmv2000' {..} =
    Prelude.rnf applicationTransactionCounter
      `Prelude.seq` Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToJSON SessionKeyEmv2000 where
  toJSON SessionKeyEmv2000' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ApplicationTransactionCounter"
                  Data..= applicationTransactionCounter
              ),
            Prelude.Just
              ("PanSequenceNumber" Data..= panSequenceNumber),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              )
          ]
      )

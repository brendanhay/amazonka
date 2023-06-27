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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyEmvCommon
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyEmvCommon where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive session key for an Emv common payment card for ARQC
-- verification.
--
-- /See:/ 'newSessionKeyEmvCommon' smart constructor.
data SessionKeyEmvCommon = SessionKeyEmvCommon'
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
-- Create a value of 'SessionKeyEmvCommon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationTransactionCounter', 'sessionKeyEmvCommon_applicationTransactionCounter' - The transaction counter that is provided by the terminal during
-- transaction processing.
--
-- 'panSequenceNumber', 'sessionKeyEmvCommon_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'primaryAccountNumber', 'sessionKeyEmvCommon_primaryAccountNumber' - The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
newSessionKeyEmvCommon ::
  -- | 'applicationTransactionCounter'
  Prelude.Text ->
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  SessionKeyEmvCommon
newSessionKeyEmvCommon
  pApplicationTransactionCounter_
  pPanSequenceNumber_
  pPrimaryAccountNumber_ =
    SessionKeyEmvCommon'
      { applicationTransactionCounter =
          pApplicationTransactionCounter_,
        panSequenceNumber = pPanSequenceNumber_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | The transaction counter that is provided by the terminal during
-- transaction processing.
sessionKeyEmvCommon_applicationTransactionCounter :: Lens.Lens' SessionKeyEmvCommon Prelude.Text
sessionKeyEmvCommon_applicationTransactionCounter = Lens.lens (\SessionKeyEmvCommon' {applicationTransactionCounter} -> applicationTransactionCounter) (\s@SessionKeyEmvCommon' {} a -> s {applicationTransactionCounter = a} :: SessionKeyEmvCommon)

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
sessionKeyEmvCommon_panSequenceNumber :: Lens.Lens' SessionKeyEmvCommon Prelude.Text
sessionKeyEmvCommon_panSequenceNumber = Lens.lens (\SessionKeyEmvCommon' {panSequenceNumber} -> panSequenceNumber) (\s@SessionKeyEmvCommon' {} a -> s {panSequenceNumber = a} :: SessionKeyEmvCommon)

-- | The Primary Account Number (PAN) of the cardholder. A PAN is a unique
-- identifier for a payment credit or debit card and associates the card to
-- a specific account holder.
sessionKeyEmvCommon_primaryAccountNumber :: Lens.Lens' SessionKeyEmvCommon Prelude.Text
sessionKeyEmvCommon_primaryAccountNumber = Lens.lens (\SessionKeyEmvCommon' {primaryAccountNumber} -> primaryAccountNumber) (\s@SessionKeyEmvCommon' {} a -> s {primaryAccountNumber = a} :: SessionKeyEmvCommon) Prelude.. Data._Sensitive

instance Prelude.Hashable SessionKeyEmvCommon where
  hashWithSalt _salt SessionKeyEmvCommon' {..} =
    _salt
      `Prelude.hashWithSalt` applicationTransactionCounter
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData SessionKeyEmvCommon where
  rnf SessionKeyEmvCommon' {..} =
    Prelude.rnf applicationTransactionCounter
      `Prelude.seq` Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToJSON SessionKeyEmvCommon where
  toJSON SessionKeyEmvCommon' {..} =
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

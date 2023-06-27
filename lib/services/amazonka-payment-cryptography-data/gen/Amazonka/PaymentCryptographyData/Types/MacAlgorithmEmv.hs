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
-- Module      : Amazonka.PaymentCryptographyData.Types.MacAlgorithmEmv
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.MacAlgorithmEmv where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.MajorKeyDerivationMode
import Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationMode
import Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationValue
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for EMV MAC generation and verification.
--
-- /See:/ 'newMacAlgorithmEmv' smart constructor.
data MacAlgorithmEmv = MacAlgorithmEmv'
  { -- | The method to use when deriving the master key for EMV MAC generation or
    -- verification.
    majorKeyDerivationMode :: MajorKeyDerivationMode,
    -- | A number that identifies and differentiates payment cards with the same
    -- Primary Account Number (PAN).
    panSequenceNumber :: Prelude.Text,
    -- | The Primary Account Number (PAN), a unique identifier for a payment
    -- credit or debit card and associates the card to a specific account
    -- holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text,
    -- | The method of deriving a session key for EMV MAC generation or
    -- verification.
    sessionKeyDerivationMode :: SessionKeyDerivationMode,
    -- | Parameters that are required to generate session key for EMV generation
    -- and verification.
    sessionKeyDerivationValue :: SessionKeyDerivationValue
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MacAlgorithmEmv' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorKeyDerivationMode', 'macAlgorithmEmv_majorKeyDerivationMode' - The method to use when deriving the master key for EMV MAC generation or
-- verification.
--
-- 'panSequenceNumber', 'macAlgorithmEmv_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'primaryAccountNumber', 'macAlgorithmEmv_primaryAccountNumber' - The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card and associates the card to a specific account
-- holder.
--
-- 'sessionKeyDerivationMode', 'macAlgorithmEmv_sessionKeyDerivationMode' - The method of deriving a session key for EMV MAC generation or
-- verification.
--
-- 'sessionKeyDerivationValue', 'macAlgorithmEmv_sessionKeyDerivationValue' - Parameters that are required to generate session key for EMV generation
-- and verification.
newMacAlgorithmEmv ::
  -- | 'majorKeyDerivationMode'
  MajorKeyDerivationMode ->
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  -- | 'sessionKeyDerivationMode'
  SessionKeyDerivationMode ->
  -- | 'sessionKeyDerivationValue'
  SessionKeyDerivationValue ->
  MacAlgorithmEmv
newMacAlgorithmEmv
  pMajorKeyDerivationMode_
  pPanSequenceNumber_
  pPrimaryAccountNumber_
  pSessionKeyDerivationMode_
  pSessionKeyDerivationValue_ =
    MacAlgorithmEmv'
      { majorKeyDerivationMode =
          pMajorKeyDerivationMode_,
        panSequenceNumber = pPanSequenceNumber_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_,
        sessionKeyDerivationMode =
          pSessionKeyDerivationMode_,
        sessionKeyDerivationValue =
          pSessionKeyDerivationValue_
      }

-- | The method to use when deriving the master key for EMV MAC generation or
-- verification.
macAlgorithmEmv_majorKeyDerivationMode :: Lens.Lens' MacAlgorithmEmv MajorKeyDerivationMode
macAlgorithmEmv_majorKeyDerivationMode = Lens.lens (\MacAlgorithmEmv' {majorKeyDerivationMode} -> majorKeyDerivationMode) (\s@MacAlgorithmEmv' {} a -> s {majorKeyDerivationMode = a} :: MacAlgorithmEmv)

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
macAlgorithmEmv_panSequenceNumber :: Lens.Lens' MacAlgorithmEmv Prelude.Text
macAlgorithmEmv_panSequenceNumber = Lens.lens (\MacAlgorithmEmv' {panSequenceNumber} -> panSequenceNumber) (\s@MacAlgorithmEmv' {} a -> s {panSequenceNumber = a} :: MacAlgorithmEmv)

-- | The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card and associates the card to a specific account
-- holder.
macAlgorithmEmv_primaryAccountNumber :: Lens.Lens' MacAlgorithmEmv Prelude.Text
macAlgorithmEmv_primaryAccountNumber = Lens.lens (\MacAlgorithmEmv' {primaryAccountNumber} -> primaryAccountNumber) (\s@MacAlgorithmEmv' {} a -> s {primaryAccountNumber = a} :: MacAlgorithmEmv) Prelude.. Data._Sensitive

-- | The method of deriving a session key for EMV MAC generation or
-- verification.
macAlgorithmEmv_sessionKeyDerivationMode :: Lens.Lens' MacAlgorithmEmv SessionKeyDerivationMode
macAlgorithmEmv_sessionKeyDerivationMode = Lens.lens (\MacAlgorithmEmv' {sessionKeyDerivationMode} -> sessionKeyDerivationMode) (\s@MacAlgorithmEmv' {} a -> s {sessionKeyDerivationMode = a} :: MacAlgorithmEmv)

-- | Parameters that are required to generate session key for EMV generation
-- and verification.
macAlgorithmEmv_sessionKeyDerivationValue :: Lens.Lens' MacAlgorithmEmv SessionKeyDerivationValue
macAlgorithmEmv_sessionKeyDerivationValue = Lens.lens (\MacAlgorithmEmv' {sessionKeyDerivationValue} -> sessionKeyDerivationValue) (\s@MacAlgorithmEmv' {} a -> s {sessionKeyDerivationValue = a} :: MacAlgorithmEmv)

instance Prelude.Hashable MacAlgorithmEmv where
  hashWithSalt _salt MacAlgorithmEmv' {..} =
    _salt
      `Prelude.hashWithSalt` majorKeyDerivationMode
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` primaryAccountNumber
      `Prelude.hashWithSalt` sessionKeyDerivationMode
      `Prelude.hashWithSalt` sessionKeyDerivationValue

instance Prelude.NFData MacAlgorithmEmv where
  rnf MacAlgorithmEmv' {..} =
    Prelude.rnf majorKeyDerivationMode
      `Prelude.seq` Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf primaryAccountNumber
      `Prelude.seq` Prelude.rnf sessionKeyDerivationMode
      `Prelude.seq` Prelude.rnf sessionKeyDerivationValue

instance Data.ToJSON MacAlgorithmEmv where
  toJSON MacAlgorithmEmv' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MajorKeyDerivationMode"
                  Data..= majorKeyDerivationMode
              ),
            Prelude.Just
              ("PanSequenceNumber" Data..= panSequenceNumber),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              ),
            Prelude.Just
              ( "SessionKeyDerivationMode"
                  Data..= sessionKeyDerivationMode
              ),
            Prelude.Just
              ( "SessionKeyDerivationValue"
                  Data..= sessionKeyDerivationValue
              )
          ]
      )

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
-- Module      : Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for ARPC response generation using method2
-- after ARQC verification is successful.
--
-- /See:/ 'newCryptogramVerificationArpcMethod2' smart constructor.
data CryptogramVerificationArpcMethod2 = CryptogramVerificationArpcMethod2'
  { -- | The proprietary authentication data used by issuer for communication
    -- during online transaction using an EMV chip card.
    proprietaryAuthenticationData :: Prelude.Maybe Prelude.Text,
    -- | The data indicating whether the issuer approves or declines an online
    -- transaction using an EMV chip card.
    cardStatusUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CryptogramVerificationArpcMethod2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proprietaryAuthenticationData', 'cryptogramVerificationArpcMethod2_proprietaryAuthenticationData' - The proprietary authentication data used by issuer for communication
-- during online transaction using an EMV chip card.
--
-- 'cardStatusUpdate', 'cryptogramVerificationArpcMethod2_cardStatusUpdate' - The data indicating whether the issuer approves or declines an online
-- transaction using an EMV chip card.
newCryptogramVerificationArpcMethod2 ::
  -- | 'cardStatusUpdate'
  Prelude.Text ->
  CryptogramVerificationArpcMethod2
newCryptogramVerificationArpcMethod2
  pCardStatusUpdate_ =
    CryptogramVerificationArpcMethod2'
      { proprietaryAuthenticationData =
          Prelude.Nothing,
        cardStatusUpdate = pCardStatusUpdate_
      }

-- | The proprietary authentication data used by issuer for communication
-- during online transaction using an EMV chip card.
cryptogramVerificationArpcMethod2_proprietaryAuthenticationData :: Lens.Lens' CryptogramVerificationArpcMethod2 (Prelude.Maybe Prelude.Text)
cryptogramVerificationArpcMethod2_proprietaryAuthenticationData = Lens.lens (\CryptogramVerificationArpcMethod2' {proprietaryAuthenticationData} -> proprietaryAuthenticationData) (\s@CryptogramVerificationArpcMethod2' {} a -> s {proprietaryAuthenticationData = a} :: CryptogramVerificationArpcMethod2)

-- | The data indicating whether the issuer approves or declines an online
-- transaction using an EMV chip card.
cryptogramVerificationArpcMethod2_cardStatusUpdate :: Lens.Lens' CryptogramVerificationArpcMethod2 Prelude.Text
cryptogramVerificationArpcMethod2_cardStatusUpdate = Lens.lens (\CryptogramVerificationArpcMethod2' {cardStatusUpdate} -> cardStatusUpdate) (\s@CryptogramVerificationArpcMethod2' {} a -> s {cardStatusUpdate = a} :: CryptogramVerificationArpcMethod2)

instance
  Prelude.Hashable
    CryptogramVerificationArpcMethod2
  where
  hashWithSalt
    _salt
    CryptogramVerificationArpcMethod2' {..} =
      _salt
        `Prelude.hashWithSalt` proprietaryAuthenticationData
        `Prelude.hashWithSalt` cardStatusUpdate

instance
  Prelude.NFData
    CryptogramVerificationArpcMethod2
  where
  rnf CryptogramVerificationArpcMethod2' {..} =
    Prelude.rnf proprietaryAuthenticationData
      `Prelude.seq` Prelude.rnf cardStatusUpdate

instance
  Data.ToJSON
    CryptogramVerificationArpcMethod2
  where
  toJSON CryptogramVerificationArpcMethod2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProprietaryAuthenticationData" Data..=)
              Prelude.<$> proprietaryAuthenticationData,
            Prelude.Just
              ("CardStatusUpdate" Data..= cardStatusUpdate)
          ]
      )

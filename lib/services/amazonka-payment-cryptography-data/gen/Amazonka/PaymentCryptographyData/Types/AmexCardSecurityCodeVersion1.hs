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
-- Module      : Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are required to generate a Card Security Code
-- (CSC2) for an AMEX payment card.
--
-- /See:/ 'newAmexCardSecurityCodeVersion1' smart constructor.
data AmexCardSecurityCodeVersion1 = AmexCardSecurityCodeVersion1'
  { -- | The expiry date of a payment card.
    cardExpiryDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmexCardSecurityCodeVersion1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cardExpiryDate', 'amexCardSecurityCodeVersion1_cardExpiryDate' - The expiry date of a payment card.
newAmexCardSecurityCodeVersion1 ::
  -- | 'cardExpiryDate'
  Prelude.Text ->
  AmexCardSecurityCodeVersion1
newAmexCardSecurityCodeVersion1 pCardExpiryDate_ =
  AmexCardSecurityCodeVersion1'
    { cardExpiryDate =
        pCardExpiryDate_
    }

-- | The expiry date of a payment card.
amexCardSecurityCodeVersion1_cardExpiryDate :: Lens.Lens' AmexCardSecurityCodeVersion1 Prelude.Text
amexCardSecurityCodeVersion1_cardExpiryDate = Lens.lens (\AmexCardSecurityCodeVersion1' {cardExpiryDate} -> cardExpiryDate) (\s@AmexCardSecurityCodeVersion1' {} a -> s {cardExpiryDate = a} :: AmexCardSecurityCodeVersion1)

instance
  Prelude.Hashable
    AmexCardSecurityCodeVersion1
  where
  hashWithSalt _salt AmexCardSecurityCodeVersion1' {..} =
    _salt `Prelude.hashWithSalt` cardExpiryDate

instance Prelude.NFData AmexCardSecurityCodeVersion1 where
  rnf AmexCardSecurityCodeVersion1' {..} =
    Prelude.rnf cardExpiryDate

instance Data.ToJSON AmexCardSecurityCodeVersion1 where
  toJSON AmexCardSecurityCodeVersion1' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CardExpiryDate" Data..= cardExpiryDate)
          ]
      )

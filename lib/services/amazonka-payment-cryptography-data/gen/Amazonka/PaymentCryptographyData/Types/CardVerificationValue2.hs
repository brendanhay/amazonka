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
-- Module      : Amazonka.PaymentCryptographyData.Types.CardVerificationValue2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CardVerificationValue2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are required to verify Card Verification Value
-- (CVV2) for the payment card.
--
-- /See:/ 'newCardVerificationValue2' smart constructor.
data CardVerificationValue2 = CardVerificationValue2'
  { -- | The expiry date of a payment card.
    cardExpiryDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CardVerificationValue2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cardExpiryDate', 'cardVerificationValue2_cardExpiryDate' - The expiry date of a payment card.
newCardVerificationValue2 ::
  -- | 'cardExpiryDate'
  Prelude.Text ->
  CardVerificationValue2
newCardVerificationValue2 pCardExpiryDate_ =
  CardVerificationValue2'
    { cardExpiryDate =
        pCardExpiryDate_
    }

-- | The expiry date of a payment card.
cardVerificationValue2_cardExpiryDate :: Lens.Lens' CardVerificationValue2 Prelude.Text
cardVerificationValue2_cardExpiryDate = Lens.lens (\CardVerificationValue2' {cardExpiryDate} -> cardExpiryDate) (\s@CardVerificationValue2' {} a -> s {cardExpiryDate = a} :: CardVerificationValue2)

instance Prelude.Hashable CardVerificationValue2 where
  hashWithSalt _salt CardVerificationValue2' {..} =
    _salt `Prelude.hashWithSalt` cardExpiryDate

instance Prelude.NFData CardVerificationValue2 where
  rnf CardVerificationValue2' {..} =
    Prelude.rnf cardExpiryDate

instance Data.ToJSON CardVerificationValue2 where
  toJSON CardVerificationValue2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CardExpiryDate" Data..= cardExpiryDate)
          ]
      )

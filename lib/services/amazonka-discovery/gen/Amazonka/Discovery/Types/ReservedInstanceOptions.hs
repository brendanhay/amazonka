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
-- Module      : Amazonka.Discovery.Types.ReservedInstanceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ReservedInstanceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.OfferingClass
import Amazonka.Discovery.Types.PurchasingOption
import Amazonka.Discovery.Types.TermLength
import qualified Amazonka.Prelude as Prelude

-- | Used to provide Reserved Instance preferences for the recommendation.
--
-- /See:/ 'newReservedInstanceOptions' smart constructor.
data ReservedInstanceOptions = ReservedInstanceOptions'
  { -- | The payment plan to use for your Reserved Instance.
    purchasingOption :: PurchasingOption,
    -- | The flexibility to change the instance types needed for your Reserved
    -- Instance.
    offeringClass :: OfferingClass,
    -- | The preferred duration of the Reserved Instance term.
    termLength :: TermLength
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'purchasingOption', 'reservedInstanceOptions_purchasingOption' - The payment plan to use for your Reserved Instance.
--
-- 'offeringClass', 'reservedInstanceOptions_offeringClass' - The flexibility to change the instance types needed for your Reserved
-- Instance.
--
-- 'termLength', 'reservedInstanceOptions_termLength' - The preferred duration of the Reserved Instance term.
newReservedInstanceOptions ::
  -- | 'purchasingOption'
  PurchasingOption ->
  -- | 'offeringClass'
  OfferingClass ->
  -- | 'termLength'
  TermLength ->
  ReservedInstanceOptions
newReservedInstanceOptions
  pPurchasingOption_
  pOfferingClass_
  pTermLength_ =
    ReservedInstanceOptions'
      { purchasingOption =
          pPurchasingOption_,
        offeringClass = pOfferingClass_,
        termLength = pTermLength_
      }

-- | The payment plan to use for your Reserved Instance.
reservedInstanceOptions_purchasingOption :: Lens.Lens' ReservedInstanceOptions PurchasingOption
reservedInstanceOptions_purchasingOption = Lens.lens (\ReservedInstanceOptions' {purchasingOption} -> purchasingOption) (\s@ReservedInstanceOptions' {} a -> s {purchasingOption = a} :: ReservedInstanceOptions)

-- | The flexibility to change the instance types needed for your Reserved
-- Instance.
reservedInstanceOptions_offeringClass :: Lens.Lens' ReservedInstanceOptions OfferingClass
reservedInstanceOptions_offeringClass = Lens.lens (\ReservedInstanceOptions' {offeringClass} -> offeringClass) (\s@ReservedInstanceOptions' {} a -> s {offeringClass = a} :: ReservedInstanceOptions)

-- | The preferred duration of the Reserved Instance term.
reservedInstanceOptions_termLength :: Lens.Lens' ReservedInstanceOptions TermLength
reservedInstanceOptions_termLength = Lens.lens (\ReservedInstanceOptions' {termLength} -> termLength) (\s@ReservedInstanceOptions' {} a -> s {termLength = a} :: ReservedInstanceOptions)

instance Prelude.Hashable ReservedInstanceOptions where
  hashWithSalt _salt ReservedInstanceOptions' {..} =
    _salt
      `Prelude.hashWithSalt` purchasingOption
      `Prelude.hashWithSalt` offeringClass
      `Prelude.hashWithSalt` termLength

instance Prelude.NFData ReservedInstanceOptions where
  rnf ReservedInstanceOptions' {..} =
    Prelude.rnf purchasingOption
      `Prelude.seq` Prelude.rnf offeringClass
      `Prelude.seq` Prelude.rnf termLength

instance Data.ToJSON ReservedInstanceOptions where
  toJSON ReservedInstanceOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("purchasingOption" Data..= purchasingOption),
            Prelude.Just ("offeringClass" Data..= offeringClass),
            Prelude.Just ("termLength" Data..= termLength)
          ]
      )

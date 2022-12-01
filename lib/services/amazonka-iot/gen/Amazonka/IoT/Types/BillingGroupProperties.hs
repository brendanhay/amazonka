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
-- Module      : Amazonka.IoT.Types.BillingGroupProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.BillingGroupProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties of a billing group.
--
-- /See:/ 'newBillingGroupProperties' smart constructor.
data BillingGroupProperties = BillingGroupProperties'
  { -- | The description of the billing group.
    billingGroupDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BillingGroupProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupDescription', 'billingGroupProperties_billingGroupDescription' - The description of the billing group.
newBillingGroupProperties ::
  BillingGroupProperties
newBillingGroupProperties =
  BillingGroupProperties'
    { billingGroupDescription =
        Prelude.Nothing
    }

-- | The description of the billing group.
billingGroupProperties_billingGroupDescription :: Lens.Lens' BillingGroupProperties (Prelude.Maybe Prelude.Text)
billingGroupProperties_billingGroupDescription = Lens.lens (\BillingGroupProperties' {billingGroupDescription} -> billingGroupDescription) (\s@BillingGroupProperties' {} a -> s {billingGroupDescription = a} :: BillingGroupProperties)

instance Core.FromJSON BillingGroupProperties where
  parseJSON =
    Core.withObject
      "BillingGroupProperties"
      ( \x ->
          BillingGroupProperties'
            Prelude.<$> (x Core..:? "billingGroupDescription")
      )

instance Prelude.Hashable BillingGroupProperties where
  hashWithSalt _salt BillingGroupProperties' {..} =
    _salt
      `Prelude.hashWithSalt` billingGroupDescription

instance Prelude.NFData BillingGroupProperties where
  rnf BillingGroupProperties' {..} =
    Prelude.rnf billingGroupDescription

instance Core.ToJSON BillingGroupProperties where
  toJSON BillingGroupProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("billingGroupDescription" Core..=)
              Prelude.<$> billingGroupDescription
          ]
      )

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
-- Module      : Network.AWS.IoT.Types.BillingGroupProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The properties of a billing group.
--
-- /See:/ 'newBillingGroupProperties' smart constructor.
data BillingGroupProperties = BillingGroupProperties'
  { -- | The description of the billing group.
    billingGroupDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The description of the billing group.
billingGroupProperties_billingGroupDescription :: Lens.Lens' BillingGroupProperties (Core.Maybe Core.Text)
billingGroupProperties_billingGroupDescription = Lens.lens (\BillingGroupProperties' {billingGroupDescription} -> billingGroupDescription) (\s@BillingGroupProperties' {} a -> s {billingGroupDescription = a} :: BillingGroupProperties)

instance Core.FromJSON BillingGroupProperties where
  parseJSON =
    Core.withObject
      "BillingGroupProperties"
      ( \x ->
          BillingGroupProperties'
            Core.<$> (x Core..:? "billingGroupDescription")
      )

instance Core.Hashable BillingGroupProperties

instance Core.NFData BillingGroupProperties

instance Core.ToJSON BillingGroupProperties where
  toJSON BillingGroupProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingGroupDescription" Core..=)
              Core.<$> billingGroupDescription
          ]
      )

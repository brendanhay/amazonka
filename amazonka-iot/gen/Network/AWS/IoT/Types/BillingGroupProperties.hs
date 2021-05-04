{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties of a billing group.
--
-- /See:/ 'newBillingGroupProperties' smart constructor.
data BillingGroupProperties = BillingGroupProperties'
  { -- | The description of the billing group.
    billingGroupDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON BillingGroupProperties where
  parseJSON =
    Prelude.withObject
      "BillingGroupProperties"
      ( \x ->
          BillingGroupProperties'
            Prelude.<$> (x Prelude..:? "billingGroupDescription")
      )

instance Prelude.Hashable BillingGroupProperties

instance Prelude.NFData BillingGroupProperties

instance Prelude.ToJSON BillingGroupProperties where
  toJSON BillingGroupProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("billingGroupDescription" Prelude..=)
              Prelude.<$> billingGroupDescription
          ]
      )

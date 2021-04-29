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
-- Module      : Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides options for how often AWS Config delivers configuration
-- snapshots to the Amazon S3 bucket in your delivery channel.
--
-- The frequency for a rule that triggers evaluations for your resources
-- when AWS Config delivers the configuration snapshot is set by one of two
-- values, depending on which is less frequent:
--
-- -   The value for the @deliveryFrequency@ parameter within the delivery
--     channel configuration, which sets how often AWS Config delivers
--     configuration snapshots. This value also sets how often AWS Config
--     invokes evaluations for AWS Config rules.
--
-- -   The value for the @MaximumExecutionFrequency@ parameter, which sets
--     the maximum frequency with which AWS Config invokes evaluations for
--     the rule. For more information, see ConfigRule.
--
-- If the @deliveryFrequency@ value is less frequent than the
-- @MaximumExecutionFrequency@ value for a rule, AWS Config invokes the
-- rule only as often as the @deliveryFrequency@ value.
--
-- 1.  For example, you want your rule to run evaluations when AWS Config
--     delivers the configuration snapshot.
--
-- 2.  You specify the @MaximumExecutionFrequency@ value for @Six_Hours@.
--
-- 3.  You then specify the delivery channel @deliveryFrequency@ value for
--     @TwentyFour_Hours@.
--
-- 4.  Because the value for @deliveryFrequency@ is less frequent than
--     @MaximumExecutionFrequency@, AWS Config invokes evaluations for the
--     rule every 24 hours.
--
-- You should set the @MaximumExecutionFrequency@ value to be at least as
-- frequent as the @deliveryFrequency@ value. You can view the
-- @deliveryFrequency@ value by using the @DescribeDeliveryChannnels@
-- action.
--
-- To update the @deliveryFrequency@ with which AWS Config delivers your
-- configuration snapshots, use the @PutDeliveryChannel@ action.
--
-- /See:/ 'newConfigSnapshotDeliveryProperties' smart constructor.
data ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties'
  { -- | The frequency with which AWS Config delivers configuration snapshots.
    deliveryFrequency :: Prelude.Maybe MaximumExecutionFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigSnapshotDeliveryProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryFrequency', 'configSnapshotDeliveryProperties_deliveryFrequency' - The frequency with which AWS Config delivers configuration snapshots.
newConfigSnapshotDeliveryProperties ::
  ConfigSnapshotDeliveryProperties
newConfigSnapshotDeliveryProperties =
  ConfigSnapshotDeliveryProperties'
    { deliveryFrequency =
        Prelude.Nothing
    }

-- | The frequency with which AWS Config delivers configuration snapshots.
configSnapshotDeliveryProperties_deliveryFrequency :: Lens.Lens' ConfigSnapshotDeliveryProperties (Prelude.Maybe MaximumExecutionFrequency)
configSnapshotDeliveryProperties_deliveryFrequency = Lens.lens (\ConfigSnapshotDeliveryProperties' {deliveryFrequency} -> deliveryFrequency) (\s@ConfigSnapshotDeliveryProperties' {} a -> s {deliveryFrequency = a} :: ConfigSnapshotDeliveryProperties)

instance
  Prelude.FromJSON
    ConfigSnapshotDeliveryProperties
  where
  parseJSON =
    Prelude.withObject
      "ConfigSnapshotDeliveryProperties"
      ( \x ->
          ConfigSnapshotDeliveryProperties'
            Prelude.<$> (x Prelude..:? "deliveryFrequency")
      )

instance
  Prelude.Hashable
    ConfigSnapshotDeliveryProperties

instance
  Prelude.NFData
    ConfigSnapshotDeliveryProperties

instance
  Prelude.ToJSON
    ConfigSnapshotDeliveryProperties
  where
  toJSON ConfigSnapshotDeliveryProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deliveryFrequency" Prelude..=)
              Prelude.<$> deliveryFrequency
          ]
      )

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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Attribute details on a specific Savings Plan.
--
-- /See:/ 'newSavingsPlansDetails' smart constructor.
data SavingsPlansDetails = SavingsPlansDetails'
  { -- | A group of instance types that Savings Plans applies to.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The unique ID used to distinguish Savings Plans from one another.
    offeringId :: Core.Maybe Core.Text,
    -- | A collection of AWS resources in a geographic area. Each AWS Region is
    -- isolated and independent of the other Regions.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'savingsPlansDetails_instanceFamily' - A group of instance types that Savings Plans applies to.
--
-- 'offeringId', 'savingsPlansDetails_offeringId' - The unique ID used to distinguish Savings Plans from one another.
--
-- 'region', 'savingsPlansDetails_region' - A collection of AWS resources in a geographic area. Each AWS Region is
-- isolated and independent of the other Regions.
newSavingsPlansDetails ::
  SavingsPlansDetails
newSavingsPlansDetails =
  SavingsPlansDetails'
    { instanceFamily = Core.Nothing,
      offeringId = Core.Nothing,
      region = Core.Nothing
    }

-- | A group of instance types that Savings Plans applies to.
savingsPlansDetails_instanceFamily :: Lens.Lens' SavingsPlansDetails (Core.Maybe Core.Text)
savingsPlansDetails_instanceFamily = Lens.lens (\SavingsPlansDetails' {instanceFamily} -> instanceFamily) (\s@SavingsPlansDetails' {} a -> s {instanceFamily = a} :: SavingsPlansDetails)

-- | The unique ID used to distinguish Savings Plans from one another.
savingsPlansDetails_offeringId :: Lens.Lens' SavingsPlansDetails (Core.Maybe Core.Text)
savingsPlansDetails_offeringId = Lens.lens (\SavingsPlansDetails' {offeringId} -> offeringId) (\s@SavingsPlansDetails' {} a -> s {offeringId = a} :: SavingsPlansDetails)

-- | A collection of AWS resources in a geographic area. Each AWS Region is
-- isolated and independent of the other Regions.
savingsPlansDetails_region :: Lens.Lens' SavingsPlansDetails (Core.Maybe Core.Text)
savingsPlansDetails_region = Lens.lens (\SavingsPlansDetails' {region} -> region) (\s@SavingsPlansDetails' {} a -> s {region = a} :: SavingsPlansDetails)

instance Core.FromJSON SavingsPlansDetails where
  parseJSON =
    Core.withObject
      "SavingsPlansDetails"
      ( \x ->
          SavingsPlansDetails'
            Core.<$> (x Core..:? "InstanceFamily")
            Core.<*> (x Core..:? "OfferingId")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable SavingsPlansDetails

instance Core.NFData SavingsPlansDetails

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
-- Module      : Network.AWS.CostExplorer.Types.ESInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ESInstanceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the Amazon ES instances that AWS recommends that you
-- purchase.
--
-- /See:/ 'newESInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { -- | The class of instance that AWS recommends.
    instanceClass :: Core.Maybe Core.Text,
    -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool,
    -- | The size of instance that AWS recommends.
    instanceSize :: Core.Maybe Core.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ESInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceClass', 'eSInstanceDetails_instanceClass' - The class of instance that AWS recommends.
--
-- 'currentGeneration', 'eSInstanceDetails_currentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- 'sizeFlexEligible', 'eSInstanceDetails_sizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- 'instanceSize', 'eSInstanceDetails_instanceSize' - The size of instance that AWS recommends.
--
-- 'region', 'eSInstanceDetails_region' - The AWS Region of the recommended reservation.
newESInstanceDetails ::
  ESInstanceDetails
newESInstanceDetails =
  ESInstanceDetails'
    { instanceClass = Core.Nothing,
      currentGeneration = Core.Nothing,
      sizeFlexEligible = Core.Nothing,
      instanceSize = Core.Nothing,
      region = Core.Nothing
    }

-- | The class of instance that AWS recommends.
eSInstanceDetails_instanceClass :: Lens.Lens' ESInstanceDetails (Core.Maybe Core.Text)
eSInstanceDetails_instanceClass = Lens.lens (\ESInstanceDetails' {instanceClass} -> instanceClass) (\s@ESInstanceDetails' {} a -> s {instanceClass = a} :: ESInstanceDetails)

-- | Whether the recommendation is for a current-generation instance.
eSInstanceDetails_currentGeneration :: Lens.Lens' ESInstanceDetails (Core.Maybe Core.Bool)
eSInstanceDetails_currentGeneration = Lens.lens (\ESInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ESInstanceDetails' {} a -> s {currentGeneration = a} :: ESInstanceDetails)

-- | Whether the recommended reservation is size flexible.
eSInstanceDetails_sizeFlexEligible :: Lens.Lens' ESInstanceDetails (Core.Maybe Core.Bool)
eSInstanceDetails_sizeFlexEligible = Lens.lens (\ESInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ESInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ESInstanceDetails)

-- | The size of instance that AWS recommends.
eSInstanceDetails_instanceSize :: Lens.Lens' ESInstanceDetails (Core.Maybe Core.Text)
eSInstanceDetails_instanceSize = Lens.lens (\ESInstanceDetails' {instanceSize} -> instanceSize) (\s@ESInstanceDetails' {} a -> s {instanceSize = a} :: ESInstanceDetails)

-- | The AWS Region of the recommended reservation.
eSInstanceDetails_region :: Lens.Lens' ESInstanceDetails (Core.Maybe Core.Text)
eSInstanceDetails_region = Lens.lens (\ESInstanceDetails' {region} -> region) (\s@ESInstanceDetails' {} a -> s {region = a} :: ESInstanceDetails)

instance Core.FromJSON ESInstanceDetails where
  parseJSON =
    Core.withObject
      "ESInstanceDetails"
      ( \x ->
          ESInstanceDetails'
            Core.<$> (x Core..:? "InstanceClass")
            Core.<*> (x Core..:? "CurrentGeneration")
            Core.<*> (x Core..:? "SizeFlexEligible")
            Core.<*> (x Core..:? "InstanceSize")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable ESInstanceDetails

instance Core.NFData ESInstanceDetails

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
-- Module      : Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DesiredWeightAndCapacity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies weight and capacity values for a production variant.
--
-- /See:/ 'newDesiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { -- | The variant\'s capacity.
    desiredInstanceCount :: Core.Maybe Core.Natural,
    -- | The variant\'s weight.
    desiredWeight :: Core.Maybe Core.Double,
    -- | The name of the variant to update.
    variantName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DesiredWeightAndCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredInstanceCount', 'desiredWeightAndCapacity_desiredInstanceCount' - The variant\'s capacity.
--
-- 'desiredWeight', 'desiredWeightAndCapacity_desiredWeight' - The variant\'s weight.
--
-- 'variantName', 'desiredWeightAndCapacity_variantName' - The name of the variant to update.
newDesiredWeightAndCapacity ::
  -- | 'variantName'
  Core.Text ->
  DesiredWeightAndCapacity
newDesiredWeightAndCapacity pVariantName_ =
  DesiredWeightAndCapacity'
    { desiredInstanceCount =
        Core.Nothing,
      desiredWeight = Core.Nothing,
      variantName = pVariantName_
    }

-- | The variant\'s capacity.
desiredWeightAndCapacity_desiredInstanceCount :: Lens.Lens' DesiredWeightAndCapacity (Core.Maybe Core.Natural)
desiredWeightAndCapacity_desiredInstanceCount = Lens.lens (\DesiredWeightAndCapacity' {desiredInstanceCount} -> desiredInstanceCount) (\s@DesiredWeightAndCapacity' {} a -> s {desiredInstanceCount = a} :: DesiredWeightAndCapacity)

-- | The variant\'s weight.
desiredWeightAndCapacity_desiredWeight :: Lens.Lens' DesiredWeightAndCapacity (Core.Maybe Core.Double)
desiredWeightAndCapacity_desiredWeight = Lens.lens (\DesiredWeightAndCapacity' {desiredWeight} -> desiredWeight) (\s@DesiredWeightAndCapacity' {} a -> s {desiredWeight = a} :: DesiredWeightAndCapacity)

-- | The name of the variant to update.
desiredWeightAndCapacity_variantName :: Lens.Lens' DesiredWeightAndCapacity Core.Text
desiredWeightAndCapacity_variantName = Lens.lens (\DesiredWeightAndCapacity' {variantName} -> variantName) (\s@DesiredWeightAndCapacity' {} a -> s {variantName = a} :: DesiredWeightAndCapacity)

instance Core.Hashable DesiredWeightAndCapacity

instance Core.NFData DesiredWeightAndCapacity

instance Core.ToJSON DesiredWeightAndCapacity where
  toJSON DesiredWeightAndCapacity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DesiredInstanceCount" Core..=)
              Core.<$> desiredInstanceCount,
            ("DesiredWeight" Core..=) Core.<$> desiredWeight,
            Core.Just ("VariantName" Core..= variantName)
          ]
      )

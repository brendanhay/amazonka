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
-- Module      : Amazonka.SageMaker.Types.DesiredWeightAndCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DesiredWeightAndCapacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies weight and capacity values for a production variant.
--
-- /See:/ 'newDesiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { -- | The variant\'s weight.
    desiredWeight :: Prelude.Maybe Prelude.Double,
    -- | The variant\'s capacity.
    desiredInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the variant to update.
    variantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DesiredWeightAndCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredWeight', 'desiredWeightAndCapacity_desiredWeight' - The variant\'s weight.
--
-- 'desiredInstanceCount', 'desiredWeightAndCapacity_desiredInstanceCount' - The variant\'s capacity.
--
-- 'variantName', 'desiredWeightAndCapacity_variantName' - The name of the variant to update.
newDesiredWeightAndCapacity ::
  -- | 'variantName'
  Prelude.Text ->
  DesiredWeightAndCapacity
newDesiredWeightAndCapacity pVariantName_ =
  DesiredWeightAndCapacity'
    { desiredWeight =
        Prelude.Nothing,
      desiredInstanceCount = Prelude.Nothing,
      variantName = pVariantName_
    }

-- | The variant\'s weight.
desiredWeightAndCapacity_desiredWeight :: Lens.Lens' DesiredWeightAndCapacity (Prelude.Maybe Prelude.Double)
desiredWeightAndCapacity_desiredWeight = Lens.lens (\DesiredWeightAndCapacity' {desiredWeight} -> desiredWeight) (\s@DesiredWeightAndCapacity' {} a -> s {desiredWeight = a} :: DesiredWeightAndCapacity)

-- | The variant\'s capacity.
desiredWeightAndCapacity_desiredInstanceCount :: Lens.Lens' DesiredWeightAndCapacity (Prelude.Maybe Prelude.Natural)
desiredWeightAndCapacity_desiredInstanceCount = Lens.lens (\DesiredWeightAndCapacity' {desiredInstanceCount} -> desiredInstanceCount) (\s@DesiredWeightAndCapacity' {} a -> s {desiredInstanceCount = a} :: DesiredWeightAndCapacity)

-- | The name of the variant to update.
desiredWeightAndCapacity_variantName :: Lens.Lens' DesiredWeightAndCapacity Prelude.Text
desiredWeightAndCapacity_variantName = Lens.lens (\DesiredWeightAndCapacity' {variantName} -> variantName) (\s@DesiredWeightAndCapacity' {} a -> s {variantName = a} :: DesiredWeightAndCapacity)

instance Prelude.Hashable DesiredWeightAndCapacity where
  hashWithSalt _salt DesiredWeightAndCapacity' {..} =
    _salt `Prelude.hashWithSalt` desiredWeight
      `Prelude.hashWithSalt` desiredInstanceCount
      `Prelude.hashWithSalt` variantName

instance Prelude.NFData DesiredWeightAndCapacity where
  rnf DesiredWeightAndCapacity' {..} =
    Prelude.rnf desiredWeight
      `Prelude.seq` Prelude.rnf desiredInstanceCount
      `Prelude.seq` Prelude.rnf variantName

instance Core.ToJSON DesiredWeightAndCapacity where
  toJSON DesiredWeightAndCapacity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DesiredWeight" Core..=) Prelude.<$> desiredWeight,
            ("DesiredInstanceCount" Core..=)
              Prelude.<$> desiredInstanceCount,
            Prelude.Just ("VariantName" Core..= variantName)
          ]
      )

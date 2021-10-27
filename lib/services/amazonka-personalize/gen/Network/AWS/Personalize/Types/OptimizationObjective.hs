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
-- Module      : Network.AWS.Personalize.Types.OptimizationObjective
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Personalize.Types.OptimizationObjective where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types.ObjectiveSensitivity
import qualified Network.AWS.Prelude as Prelude

-- | Describes the additional objective for the solution, such as maximizing
-- streaming minutes or increasing revenue. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/optimizing-solution-for-objective.html Optimizing a solution>.
--
-- /See:/ 'newOptimizationObjective' smart constructor.
data OptimizationObjective = OptimizationObjective'
  { -- | The numerical metadata column in an Items dataset related to the
    -- optimization objective. For example, VIDEO_LENGTH (to maximize streaming
    -- minutes), or PRICE (to maximize revenue).
    itemAttribute :: Prelude.Maybe Prelude.Text,
    -- | Specifies how Amazon Personalize balances the importance of your
    -- optimization objective versus relevance.
    objectiveSensitivity :: Prelude.Maybe ObjectiveSensitivity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptimizationObjective' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemAttribute', 'optimizationObjective_itemAttribute' - The numerical metadata column in an Items dataset related to the
-- optimization objective. For example, VIDEO_LENGTH (to maximize streaming
-- minutes), or PRICE (to maximize revenue).
--
-- 'objectiveSensitivity', 'optimizationObjective_objectiveSensitivity' - Specifies how Amazon Personalize balances the importance of your
-- optimization objective versus relevance.
newOptimizationObjective ::
  OptimizationObjective
newOptimizationObjective =
  OptimizationObjective'
    { itemAttribute =
        Prelude.Nothing,
      objectiveSensitivity = Prelude.Nothing
    }

-- | The numerical metadata column in an Items dataset related to the
-- optimization objective. For example, VIDEO_LENGTH (to maximize streaming
-- minutes), or PRICE (to maximize revenue).
optimizationObjective_itemAttribute :: Lens.Lens' OptimizationObjective (Prelude.Maybe Prelude.Text)
optimizationObjective_itemAttribute = Lens.lens (\OptimizationObjective' {itemAttribute} -> itemAttribute) (\s@OptimizationObjective' {} a -> s {itemAttribute = a} :: OptimizationObjective)

-- | Specifies how Amazon Personalize balances the importance of your
-- optimization objective versus relevance.
optimizationObjective_objectiveSensitivity :: Lens.Lens' OptimizationObjective (Prelude.Maybe ObjectiveSensitivity)
optimizationObjective_objectiveSensitivity = Lens.lens (\OptimizationObjective' {objectiveSensitivity} -> objectiveSensitivity) (\s@OptimizationObjective' {} a -> s {objectiveSensitivity = a} :: OptimizationObjective)

instance Core.FromJSON OptimizationObjective where
  parseJSON =
    Core.withObject
      "OptimizationObjective"
      ( \x ->
          OptimizationObjective'
            Prelude.<$> (x Core..:? "itemAttribute")
            Prelude.<*> (x Core..:? "objectiveSensitivity")
      )

instance Prelude.Hashable OptimizationObjective

instance Prelude.NFData OptimizationObjective

instance Core.ToJSON OptimizationObjective where
  toJSON OptimizationObjective' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("itemAttribute" Core..=) Prelude.<$> itemAttribute,
            ("objectiveSensitivity" Core..=)
              Prelude.<$> objectiveSensitivity
          ]
      )

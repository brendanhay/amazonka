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
-- Module      : Amazonka.Personalize.Types.OptimizationObjective
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.OptimizationObjective where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.ObjectiveSensitivity
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON OptimizationObjective where
  parseJSON =
    Data.withObject
      "OptimizationObjective"
      ( \x ->
          OptimizationObjective'
            Prelude.<$> (x Data..:? "itemAttribute")
            Prelude.<*> (x Data..:? "objectiveSensitivity")
      )

instance Prelude.Hashable OptimizationObjective where
  hashWithSalt _salt OptimizationObjective' {..} =
    _salt
      `Prelude.hashWithSalt` itemAttribute
      `Prelude.hashWithSalt` objectiveSensitivity

instance Prelude.NFData OptimizationObjective where
  rnf OptimizationObjective' {..} =
    Prelude.rnf itemAttribute
      `Prelude.seq` Prelude.rnf objectiveSensitivity

instance Data.ToJSON OptimizationObjective where
  toJSON OptimizationObjective' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("itemAttribute" Data..=) Prelude.<$> itemAttribute,
            ("objectiveSensitivity" Data..=)
              Prelude.<$> objectiveSensitivity
          ]
      )

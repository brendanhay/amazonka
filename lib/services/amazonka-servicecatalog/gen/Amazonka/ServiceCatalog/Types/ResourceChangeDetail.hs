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
-- Module      : Amazonka.ServiceCatalog.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ResourceChangeDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.EvaluationType
import Amazonka.ServiceCatalog.Types.ResourceTargetDefinition

-- | Information about a change to a resource attribute.
--
-- /See:/ 'newResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { -- | Information about the resource attribute to be modified.
    target :: Prelude.Maybe ResourceTargetDefinition,
    -- | For static evaluations, the value of the resource attribute will change
    -- and the new value is known. For dynamic evaluations, the value might
    -- change, and any new value will be determined when the plan is updated.
    evaluation :: Prelude.Maybe EvaluationType,
    -- | The ID of the entity that caused the change.
    causingEntity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceChangeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'resourceChangeDetail_target' - Information about the resource attribute to be modified.
--
-- 'evaluation', 'resourceChangeDetail_evaluation' - For static evaluations, the value of the resource attribute will change
-- and the new value is known. For dynamic evaluations, the value might
-- change, and any new value will be determined when the plan is updated.
--
-- 'causingEntity', 'resourceChangeDetail_causingEntity' - The ID of the entity that caused the change.
newResourceChangeDetail ::
  ResourceChangeDetail
newResourceChangeDetail =
  ResourceChangeDetail'
    { target = Prelude.Nothing,
      evaluation = Prelude.Nothing,
      causingEntity = Prelude.Nothing
    }

-- | Information about the resource attribute to be modified.
resourceChangeDetail_target :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe ResourceTargetDefinition)
resourceChangeDetail_target = Lens.lens (\ResourceChangeDetail' {target} -> target) (\s@ResourceChangeDetail' {} a -> s {target = a} :: ResourceChangeDetail)

-- | For static evaluations, the value of the resource attribute will change
-- and the new value is known. For dynamic evaluations, the value might
-- change, and any new value will be determined when the plan is updated.
resourceChangeDetail_evaluation :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe EvaluationType)
resourceChangeDetail_evaluation = Lens.lens (\ResourceChangeDetail' {evaluation} -> evaluation) (\s@ResourceChangeDetail' {} a -> s {evaluation = a} :: ResourceChangeDetail)

-- | The ID of the entity that caused the change.
resourceChangeDetail_causingEntity :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe Prelude.Text)
resourceChangeDetail_causingEntity = Lens.lens (\ResourceChangeDetail' {causingEntity} -> causingEntity) (\s@ResourceChangeDetail' {} a -> s {causingEntity = a} :: ResourceChangeDetail)

instance Data.FromJSON ResourceChangeDetail where
  parseJSON =
    Data.withObject
      "ResourceChangeDetail"
      ( \x ->
          ResourceChangeDetail'
            Prelude.<$> (x Data..:? "Target")
            Prelude.<*> (x Data..:? "Evaluation")
            Prelude.<*> (x Data..:? "CausingEntity")
      )

instance Prelude.Hashable ResourceChangeDetail where
  hashWithSalt _salt ResourceChangeDetail' {..} =
    _salt `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` evaluation
      `Prelude.hashWithSalt` causingEntity

instance Prelude.NFData ResourceChangeDetail where
  rnf ResourceChangeDetail' {..} =
    Prelude.rnf target
      `Prelude.seq` Prelude.rnf evaluation
      `Prelude.seq` Prelude.rnf causingEntity

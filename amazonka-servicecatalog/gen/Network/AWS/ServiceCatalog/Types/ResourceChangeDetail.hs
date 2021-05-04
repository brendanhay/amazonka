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
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChangeDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.EvaluationType
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition

-- | Information about a change to a resource attribute.
--
-- /See:/ 'newResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { -- | For static evaluations, the value of the resource attribute will change
    -- and the new value is known. For dynamic evaluations, the value might
    -- change, and any new value will be determined when the plan is updated.
    evaluation :: Prelude.Maybe EvaluationType,
    -- | The ID of the entity that caused the change.
    causingEntity :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource attribute to be modified.
    target :: Prelude.Maybe ResourceTargetDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceChangeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluation', 'resourceChangeDetail_evaluation' - For static evaluations, the value of the resource attribute will change
-- and the new value is known. For dynamic evaluations, the value might
-- change, and any new value will be determined when the plan is updated.
--
-- 'causingEntity', 'resourceChangeDetail_causingEntity' - The ID of the entity that caused the change.
--
-- 'target', 'resourceChangeDetail_target' - Information about the resource attribute to be modified.
newResourceChangeDetail ::
  ResourceChangeDetail
newResourceChangeDetail =
  ResourceChangeDetail'
    { evaluation = Prelude.Nothing,
      causingEntity = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | For static evaluations, the value of the resource attribute will change
-- and the new value is known. For dynamic evaluations, the value might
-- change, and any new value will be determined when the plan is updated.
resourceChangeDetail_evaluation :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe EvaluationType)
resourceChangeDetail_evaluation = Lens.lens (\ResourceChangeDetail' {evaluation} -> evaluation) (\s@ResourceChangeDetail' {} a -> s {evaluation = a} :: ResourceChangeDetail)

-- | The ID of the entity that caused the change.
resourceChangeDetail_causingEntity :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe Prelude.Text)
resourceChangeDetail_causingEntity = Lens.lens (\ResourceChangeDetail' {causingEntity} -> causingEntity) (\s@ResourceChangeDetail' {} a -> s {causingEntity = a} :: ResourceChangeDetail)

-- | Information about the resource attribute to be modified.
resourceChangeDetail_target :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe ResourceTargetDefinition)
resourceChangeDetail_target = Lens.lens (\ResourceChangeDetail' {target} -> target) (\s@ResourceChangeDetail' {} a -> s {target = a} :: ResourceChangeDetail)

instance Prelude.FromJSON ResourceChangeDetail where
  parseJSON =
    Prelude.withObject
      "ResourceChangeDetail"
      ( \x ->
          ResourceChangeDetail'
            Prelude.<$> (x Prelude..:? "Evaluation")
            Prelude.<*> (x Prelude..:? "CausingEntity")
            Prelude.<*> (x Prelude..:? "Target")
      )

instance Prelude.Hashable ResourceChangeDetail

instance Prelude.NFData ResourceChangeDetail

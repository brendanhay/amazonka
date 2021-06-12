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
-- Module      : Network.AWS.GuardDuty.Types.FindingCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.Condition
import qualified Network.AWS.Lens as Lens

-- | Contains information about the criteria used for querying findings.
--
-- /See:/ 'newFindingCriteria' smart constructor.
data FindingCriteria = FindingCriteria'
  { -- | Represents a map of finding properties that match specified conditions
    -- and values when querying findings.
    criterion :: Core.Maybe (Core.HashMap Core.Text Condition)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FindingCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criterion', 'findingCriteria_criterion' - Represents a map of finding properties that match specified conditions
-- and values when querying findings.
newFindingCriteria ::
  FindingCriteria
newFindingCriteria =
  FindingCriteria' {criterion = Core.Nothing}

-- | Represents a map of finding properties that match specified conditions
-- and values when querying findings.
findingCriteria_criterion :: Lens.Lens' FindingCriteria (Core.Maybe (Core.HashMap Core.Text Condition))
findingCriteria_criterion = Lens.lens (\FindingCriteria' {criterion} -> criterion) (\s@FindingCriteria' {} a -> s {criterion = a} :: FindingCriteria) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON FindingCriteria where
  parseJSON =
    Core.withObject
      "FindingCriteria"
      ( \x ->
          FindingCriteria'
            Core.<$> (x Core..:? "criterion" Core..!= Core.mempty)
      )

instance Core.Hashable FindingCriteria

instance Core.NFData FindingCriteria

instance Core.ToJSON FindingCriteria where
  toJSON FindingCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [("criterion" Core..=) Core.<$> criterion]
      )

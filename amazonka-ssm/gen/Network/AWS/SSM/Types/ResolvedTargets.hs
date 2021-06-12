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
-- Module      : Network.AWS.SSM.Types.ResolvedTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResolvedTargets where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about targets that resolved during the Automation execution.
--
-- /See:/ 'newResolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { -- | A list of parameter values sent to targets that resolved during the
    -- Automation execution.
    parameterValues :: Core.Maybe [Core.Text],
    -- | A boolean value indicating whether the resolved target list is
    -- truncated.
    truncated :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResolvedTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValues', 'resolvedTargets_parameterValues' - A list of parameter values sent to targets that resolved during the
-- Automation execution.
--
-- 'truncated', 'resolvedTargets_truncated' - A boolean value indicating whether the resolved target list is
-- truncated.
newResolvedTargets ::
  ResolvedTargets
newResolvedTargets =
  ResolvedTargets'
    { parameterValues = Core.Nothing,
      truncated = Core.Nothing
    }

-- | A list of parameter values sent to targets that resolved during the
-- Automation execution.
resolvedTargets_parameterValues :: Lens.Lens' ResolvedTargets (Core.Maybe [Core.Text])
resolvedTargets_parameterValues = Lens.lens (\ResolvedTargets' {parameterValues} -> parameterValues) (\s@ResolvedTargets' {} a -> s {parameterValues = a} :: ResolvedTargets) Core.. Lens.mapping Lens._Coerce

-- | A boolean value indicating whether the resolved target list is
-- truncated.
resolvedTargets_truncated :: Lens.Lens' ResolvedTargets (Core.Maybe Core.Bool)
resolvedTargets_truncated = Lens.lens (\ResolvedTargets' {truncated} -> truncated) (\s@ResolvedTargets' {} a -> s {truncated = a} :: ResolvedTargets)

instance Core.FromJSON ResolvedTargets where
  parseJSON =
    Core.withObject
      "ResolvedTargets"
      ( \x ->
          ResolvedTargets'
            Core.<$> (x Core..:? "ParameterValues" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Truncated")
      )

instance Core.Hashable ResolvedTargets

instance Core.NFData ResolvedTargets

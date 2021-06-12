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
-- Module      : Network.AWS.StepFunctions.Types.TracingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TracingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Selects whether or not the state machine\'s AWS X-Ray tracing is
-- enabled. Default is @false@
--
-- /See:/ 'newTracingConfiguration' smart constructor.
data TracingConfiguration = TracingConfiguration'
  { -- | When set to @true@, AWS X-Ray tracing is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TracingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'tracingConfiguration_enabled' - When set to @true@, AWS X-Ray tracing is enabled.
newTracingConfiguration ::
  TracingConfiguration
newTracingConfiguration =
  TracingConfiguration' {enabled = Core.Nothing}

-- | When set to @true@, AWS X-Ray tracing is enabled.
tracingConfiguration_enabled :: Lens.Lens' TracingConfiguration (Core.Maybe Core.Bool)
tracingConfiguration_enabled = Lens.lens (\TracingConfiguration' {enabled} -> enabled) (\s@TracingConfiguration' {} a -> s {enabled = a} :: TracingConfiguration)

instance Core.FromJSON TracingConfiguration where
  parseJSON =
    Core.withObject
      "TracingConfiguration"
      ( \x ->
          TracingConfiguration'
            Core.<$> (x Core..:? "enabled")
      )

instance Core.Hashable TracingConfiguration

instance Core.NFData TracingConfiguration

instance Core.ToJSON TracingConfiguration where
  toJSON TracingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [("enabled" Core..=) Core.<$> enabled]
      )

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
-- Module      : Network.AWS.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about an action configuration.
--
-- /See:/ 'newActionConfiguration' smart constructor.
data ActionConfiguration = ActionConfiguration'
  { -- | The configuration data for the action.
    configuration :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'actionConfiguration_configuration' - The configuration data for the action.
newActionConfiguration ::
  ActionConfiguration
newActionConfiguration =
  ActionConfiguration' {configuration = Core.Nothing}

-- | The configuration data for the action.
actionConfiguration_configuration :: Lens.Lens' ActionConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
actionConfiguration_configuration = Lens.lens (\ActionConfiguration' {configuration} -> configuration) (\s@ActionConfiguration' {} a -> s {configuration = a} :: ActionConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ActionConfiguration where
  parseJSON =
    Core.withObject
      "ActionConfiguration"
      ( \x ->
          ActionConfiguration'
            Core.<$> (x Core..:? "configuration" Core..!= Core.mempty)
      )

instance Core.Hashable ActionConfiguration

instance Core.NFData ActionConfiguration

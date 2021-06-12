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
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | The name of the launch configuration.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'launchConfiguration_name' - The name of the launch configuration.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration' {name = Core.Nothing}

-- | The name of the launch configuration.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Core.Maybe Core.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

instance Core.FromXML LaunchConfiguration where
  parseXML x =
    LaunchConfiguration' Core.<$> (x Core..@? "Name")

instance Core.Hashable LaunchConfiguration

instance Core.NFData LaunchConfiguration

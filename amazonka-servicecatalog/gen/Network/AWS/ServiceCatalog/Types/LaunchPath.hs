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
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPath where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A launch path object.
--
-- /See:/ 'newLaunchPath' smart constructor.
data LaunchPath = LaunchPath'
  { -- | The identifier of the launch path.
    id :: Core.Maybe Core.Text,
    -- | The name of the launch path.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'launchPath_id' - The identifier of the launch path.
--
-- 'name', 'launchPath_name' - The name of the launch path.
newLaunchPath ::
  LaunchPath
newLaunchPath =
  LaunchPath' {id = Core.Nothing, name = Core.Nothing}

-- | The identifier of the launch path.
launchPath_id :: Lens.Lens' LaunchPath (Core.Maybe Core.Text)
launchPath_id = Lens.lens (\LaunchPath' {id} -> id) (\s@LaunchPath' {} a -> s {id = a} :: LaunchPath)

-- | The name of the launch path.
launchPath_name :: Lens.Lens' LaunchPath (Core.Maybe Core.Text)
launchPath_name = Lens.lens (\LaunchPath' {name} -> name) (\s@LaunchPath' {} a -> s {name = a} :: LaunchPath)

instance Core.FromJSON LaunchPath where
  parseJSON =
    Core.withObject
      "LaunchPath"
      ( \x ->
          LaunchPath'
            Core.<$> (x Core..:? "Id") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable LaunchPath

instance Core.NFData LaunchPath

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
-- Module      : Network.AWS.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LaunchDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the latest launch of an application.
--
-- /See:/ 'newLaunchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { -- | The name of the latest stack launched for this application.
    stackName :: Core.Maybe Core.Text,
    -- | The ID of the latest stack launched for this application.
    stackId :: Core.Maybe Core.Text,
    -- | The latest time that this application was launched successfully.
    latestLaunchTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'launchDetails_stackName' - The name of the latest stack launched for this application.
--
-- 'stackId', 'launchDetails_stackId' - The ID of the latest stack launched for this application.
--
-- 'latestLaunchTime', 'launchDetails_latestLaunchTime' - The latest time that this application was launched successfully.
newLaunchDetails ::
  LaunchDetails
newLaunchDetails =
  LaunchDetails'
    { stackName = Core.Nothing,
      stackId = Core.Nothing,
      latestLaunchTime = Core.Nothing
    }

-- | The name of the latest stack launched for this application.
launchDetails_stackName :: Lens.Lens' LaunchDetails (Core.Maybe Core.Text)
launchDetails_stackName = Lens.lens (\LaunchDetails' {stackName} -> stackName) (\s@LaunchDetails' {} a -> s {stackName = a} :: LaunchDetails)

-- | The ID of the latest stack launched for this application.
launchDetails_stackId :: Lens.Lens' LaunchDetails (Core.Maybe Core.Text)
launchDetails_stackId = Lens.lens (\LaunchDetails' {stackId} -> stackId) (\s@LaunchDetails' {} a -> s {stackId = a} :: LaunchDetails)

-- | The latest time that this application was launched successfully.
launchDetails_latestLaunchTime :: Lens.Lens' LaunchDetails (Core.Maybe Core.UTCTime)
launchDetails_latestLaunchTime = Lens.lens (\LaunchDetails' {latestLaunchTime} -> latestLaunchTime) (\s@LaunchDetails' {} a -> s {latestLaunchTime = a} :: LaunchDetails) Core.. Lens.mapping Core._Time

instance Core.FromJSON LaunchDetails where
  parseJSON =
    Core.withObject
      "LaunchDetails"
      ( \x ->
          LaunchDetails'
            Core.<$> (x Core..:? "stackName")
            Core.<*> (x Core..:? "stackId")
            Core.<*> (x Core..:? "latestLaunchTime")
      )

instance Core.Hashable LaunchDetails

instance Core.NFData LaunchDetails

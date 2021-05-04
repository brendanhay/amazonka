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
-- Module      : Network.AWS.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LaunchDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the latest launch of an application.
--
-- /See:/ 'newLaunchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { -- | The name of the latest stack launched for this application.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest stack launched for this application.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The latest time that this application was launched successfully.
    latestLaunchTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stackName = Prelude.Nothing,
      stackId = Prelude.Nothing,
      latestLaunchTime = Prelude.Nothing
    }

-- | The name of the latest stack launched for this application.
launchDetails_stackName :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.Text)
launchDetails_stackName = Lens.lens (\LaunchDetails' {stackName} -> stackName) (\s@LaunchDetails' {} a -> s {stackName = a} :: LaunchDetails)

-- | The ID of the latest stack launched for this application.
launchDetails_stackId :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.Text)
launchDetails_stackId = Lens.lens (\LaunchDetails' {stackId} -> stackId) (\s@LaunchDetails' {} a -> s {stackId = a} :: LaunchDetails)

-- | The latest time that this application was launched successfully.
launchDetails_latestLaunchTime :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.UTCTime)
launchDetails_latestLaunchTime = Lens.lens (\LaunchDetails' {latestLaunchTime} -> latestLaunchTime) (\s@LaunchDetails' {} a -> s {latestLaunchTime = a} :: LaunchDetails) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON LaunchDetails where
  parseJSON =
    Prelude.withObject
      "LaunchDetails"
      ( \x ->
          LaunchDetails'
            Prelude.<$> (x Prelude..:? "stackName")
            Prelude.<*> (x Prelude..:? "stackId")
            Prelude.<*> (x Prelude..:? "latestLaunchTime")
      )

instance Prelude.Hashable LaunchDetails

instance Prelude.NFData LaunchDetails

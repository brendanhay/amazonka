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
-- Module      : Network.AWS.MGN.Types.LaunchedInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.LaunchedInstance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types.FirstBoot
import qualified Network.AWS.Prelude as Prelude

-- | Configure launced instance.
--
-- /See:/ 'newLaunchedInstance' smart constructor.
data LaunchedInstance = LaunchedInstance'
  { -- | Configure launced instance Job ID.
    jobID :: Prelude.Maybe Prelude.Text,
    -- | Configure launced instance EC2 ID.
    ec2InstanceID :: Prelude.Maybe Prelude.Text,
    -- | Configure launced instance first boot.
    firstBoot :: Prelude.Maybe FirstBoot
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchedInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobID', 'launchedInstance_jobID' - Configure launced instance Job ID.
--
-- 'ec2InstanceID', 'launchedInstance_ec2InstanceID' - Configure launced instance EC2 ID.
--
-- 'firstBoot', 'launchedInstance_firstBoot' - Configure launced instance first boot.
newLaunchedInstance ::
  LaunchedInstance
newLaunchedInstance =
  LaunchedInstance'
    { jobID = Prelude.Nothing,
      ec2InstanceID = Prelude.Nothing,
      firstBoot = Prelude.Nothing
    }

-- | Configure launced instance Job ID.
launchedInstance_jobID :: Lens.Lens' LaunchedInstance (Prelude.Maybe Prelude.Text)
launchedInstance_jobID = Lens.lens (\LaunchedInstance' {jobID} -> jobID) (\s@LaunchedInstance' {} a -> s {jobID = a} :: LaunchedInstance)

-- | Configure launced instance EC2 ID.
launchedInstance_ec2InstanceID :: Lens.Lens' LaunchedInstance (Prelude.Maybe Prelude.Text)
launchedInstance_ec2InstanceID = Lens.lens (\LaunchedInstance' {ec2InstanceID} -> ec2InstanceID) (\s@LaunchedInstance' {} a -> s {ec2InstanceID = a} :: LaunchedInstance)

-- | Configure launced instance first boot.
launchedInstance_firstBoot :: Lens.Lens' LaunchedInstance (Prelude.Maybe FirstBoot)
launchedInstance_firstBoot = Lens.lens (\LaunchedInstance' {firstBoot} -> firstBoot) (\s@LaunchedInstance' {} a -> s {firstBoot = a} :: LaunchedInstance)

instance Core.FromJSON LaunchedInstance where
  parseJSON =
    Core.withObject
      "LaunchedInstance"
      ( \x ->
          LaunchedInstance'
            Prelude.<$> (x Core..:? "jobID")
            Prelude.<*> (x Core..:? "ec2InstanceID")
            Prelude.<*> (x Core..:? "firstBoot")
      )

instance Prelude.Hashable LaunchedInstance

instance Prelude.NFData LaunchedInstance

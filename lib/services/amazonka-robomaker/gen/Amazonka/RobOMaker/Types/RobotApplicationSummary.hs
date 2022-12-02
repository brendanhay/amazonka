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
-- Module      : Amazonka.RobOMaker.Types.RobotApplicationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.RobotSoftwareSuite

-- | Summary information for a robot application.
--
-- /See:/ 'newRobotApplicationSummary' smart constructor.
data RobotApplicationSummary = RobotApplicationSummary'
  { -- | The name of the robot application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the robot application
    -- was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the robot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about a robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The version of the robot application.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RobotApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'robotApplicationSummary_name' - The name of the robot application.
--
-- 'lastUpdatedAt', 'robotApplicationSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the robot application
-- was last updated.
--
-- 'arn', 'robotApplicationSummary_arn' - The Amazon Resource Name (ARN) of the robot.
--
-- 'robotSoftwareSuite', 'robotApplicationSummary_robotSoftwareSuite' - Information about a robot software suite (ROS distribution).
--
-- 'version', 'robotApplicationSummary_version' - The version of the robot application.
newRobotApplicationSummary ::
  RobotApplicationSummary
newRobotApplicationSummary =
  RobotApplicationSummary'
    { name = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the robot application.
robotApplicationSummary_name :: Lens.Lens' RobotApplicationSummary (Prelude.Maybe Prelude.Text)
robotApplicationSummary_name = Lens.lens (\RobotApplicationSummary' {name} -> name) (\s@RobotApplicationSummary' {} a -> s {name = a} :: RobotApplicationSummary)

-- | The time, in milliseconds since the epoch, when the robot application
-- was last updated.
robotApplicationSummary_lastUpdatedAt :: Lens.Lens' RobotApplicationSummary (Prelude.Maybe Prelude.UTCTime)
robotApplicationSummary_lastUpdatedAt = Lens.lens (\RobotApplicationSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@RobotApplicationSummary' {} a -> s {lastUpdatedAt = a} :: RobotApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the robot.
robotApplicationSummary_arn :: Lens.Lens' RobotApplicationSummary (Prelude.Maybe Prelude.Text)
robotApplicationSummary_arn = Lens.lens (\RobotApplicationSummary' {arn} -> arn) (\s@RobotApplicationSummary' {} a -> s {arn = a} :: RobotApplicationSummary)

-- | Information about a robot software suite (ROS distribution).
robotApplicationSummary_robotSoftwareSuite :: Lens.Lens' RobotApplicationSummary (Prelude.Maybe RobotSoftwareSuite)
robotApplicationSummary_robotSoftwareSuite = Lens.lens (\RobotApplicationSummary' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@RobotApplicationSummary' {} a -> s {robotSoftwareSuite = a} :: RobotApplicationSummary)

-- | The version of the robot application.
robotApplicationSummary_version :: Lens.Lens' RobotApplicationSummary (Prelude.Maybe Prelude.Text)
robotApplicationSummary_version = Lens.lens (\RobotApplicationSummary' {version} -> version) (\s@RobotApplicationSummary' {} a -> s {version = a} :: RobotApplicationSummary)

instance Data.FromJSON RobotApplicationSummary where
  parseJSON =
    Data.withObject
      "RobotApplicationSummary"
      ( \x ->
          RobotApplicationSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "robotSoftwareSuite")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable RobotApplicationSummary where
  hashWithSalt _salt RobotApplicationSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` robotSoftwareSuite
      `Prelude.hashWithSalt` version

instance Prelude.NFData RobotApplicationSummary where
  rnf RobotApplicationSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf version

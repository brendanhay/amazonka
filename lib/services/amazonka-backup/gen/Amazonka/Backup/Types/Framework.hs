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
-- Module      : Amazonka.Backup.Types.Framework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Framework where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a framework. Frameworks contain
-- controls, which evaluate and report on your backup events and resources.
-- Frameworks generate daily compliance results.
--
-- /See:/ 'newFramework' smart constructor.
data Framework = Framework'
  { -- | The date and time that a framework is created, in ISO 8601
    -- representation. The value of @CreationTime@ is accurate to milliseconds.
    -- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
    -- 2020 at 3:00 PM 8 hours behind UTC.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The deployment status of a framework. The statuses are:
    --
    -- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    frameworkArn :: Prelude.Maybe Prelude.Text,
    -- | An optional description of the framework with a maximum 1,024
    -- characters.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a framework. This name is between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The number of controls contained by the framework.
    numberOfControls :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Framework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'framework_creationTime' - The date and time that a framework is created, in ISO 8601
-- representation. The value of @CreationTime@ is accurate to milliseconds.
-- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
-- 2020 at 3:00 PM 8 hours behind UTC.
--
-- 'deploymentStatus', 'framework_deploymentStatus' - The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
--
-- 'frameworkArn', 'framework_frameworkArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'frameworkDescription', 'framework_frameworkDescription' - An optional description of the framework with a maximum 1,024
-- characters.
--
-- 'frameworkName', 'framework_frameworkName' - The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
--
-- 'numberOfControls', 'framework_numberOfControls' - The number of controls contained by the framework.
newFramework ::
  Framework
newFramework =
  Framework'
    { creationTime = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      frameworkArn = Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      numberOfControls = Prelude.Nothing
    }

-- | The date and time that a framework is created, in ISO 8601
-- representation. The value of @CreationTime@ is accurate to milliseconds.
-- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
-- 2020 at 3:00 PM 8 hours behind UTC.
framework_creationTime :: Lens.Lens' Framework (Prelude.Maybe Prelude.UTCTime)
framework_creationTime = Lens.lens (\Framework' {creationTime} -> creationTime) (\s@Framework' {} a -> s {creationTime = a} :: Framework) Prelude.. Lens.mapping Data._Time

-- | The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
framework_deploymentStatus :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_deploymentStatus = Lens.lens (\Framework' {deploymentStatus} -> deploymentStatus) (\s@Framework' {} a -> s {deploymentStatus = a} :: Framework)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
framework_frameworkArn :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkArn = Lens.lens (\Framework' {frameworkArn} -> frameworkArn) (\s@Framework' {} a -> s {frameworkArn = a} :: Framework)

-- | An optional description of the framework with a maximum 1,024
-- characters.
framework_frameworkDescription :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkDescription = Lens.lens (\Framework' {frameworkDescription} -> frameworkDescription) (\s@Framework' {} a -> s {frameworkDescription = a} :: Framework)

-- | The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
framework_frameworkName :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkName = Lens.lens (\Framework' {frameworkName} -> frameworkName) (\s@Framework' {} a -> s {frameworkName = a} :: Framework)

-- | The number of controls contained by the framework.
framework_numberOfControls :: Lens.Lens' Framework (Prelude.Maybe Prelude.Int)
framework_numberOfControls = Lens.lens (\Framework' {numberOfControls} -> numberOfControls) (\s@Framework' {} a -> s {numberOfControls = a} :: Framework)

instance Data.FromJSON Framework where
  parseJSON =
    Data.withObject
      "Framework"
      ( \x ->
          Framework'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DeploymentStatus")
            Prelude.<*> (x Data..:? "FrameworkArn")
            Prelude.<*> (x Data..:? "FrameworkDescription")
            Prelude.<*> (x Data..:? "FrameworkName")
            Prelude.<*> (x Data..:? "NumberOfControls")
      )

instance Prelude.Hashable Framework where
  hashWithSalt _salt Framework' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` frameworkArn
      `Prelude.hashWithSalt` frameworkDescription
      `Prelude.hashWithSalt` frameworkName
      `Prelude.hashWithSalt` numberOfControls

instance Prelude.NFData Framework where
  rnf Framework' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf frameworkArn
      `Prelude.seq` Prelude.rnf frameworkDescription
      `Prelude.seq` Prelude.rnf frameworkName
      `Prelude.seq` Prelude.rnf numberOfControls

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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Framework where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a framework. Frameworks contain
-- controls, which evaluate and report on your backup events and resources.
-- Frameworks generate daily compliance results.
--
-- /See:/ 'newFramework' smart constructor.
data Framework = Framework'
  { -- | The date and time that a framework is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | An optional description of the framework with a maximum 1,024
    -- characters.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | The number of controls contained by the framework.
    numberOfControls :: Prelude.Maybe Prelude.Int,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    frameworkArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment status of a framework. The statuses are:
    --
    -- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a framework. This name is between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'framework_creationTime' - The date and time that a framework is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'frameworkDescription', 'framework_frameworkDescription' - An optional description of the framework with a maximum 1,024
-- characters.
--
-- 'numberOfControls', 'framework_numberOfControls' - The number of controls contained by the framework.
--
-- 'frameworkArn', 'framework_frameworkArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'deploymentStatus', 'framework_deploymentStatus' - The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
--
-- 'frameworkName', 'framework_frameworkName' - The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
newFramework ::
  Framework
newFramework =
  Framework'
    { creationTime = Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      numberOfControls = Prelude.Nothing,
      frameworkArn = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      frameworkName = Prelude.Nothing
    }

-- | The date and time that a framework is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
framework_creationTime :: Lens.Lens' Framework (Prelude.Maybe Prelude.UTCTime)
framework_creationTime = Lens.lens (\Framework' {creationTime} -> creationTime) (\s@Framework' {} a -> s {creationTime = a} :: Framework) Prelude.. Lens.mapping Core._Time

-- | An optional description of the framework with a maximum 1,024
-- characters.
framework_frameworkDescription :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkDescription = Lens.lens (\Framework' {frameworkDescription} -> frameworkDescription) (\s@Framework' {} a -> s {frameworkDescription = a} :: Framework)

-- | The number of controls contained by the framework.
framework_numberOfControls :: Lens.Lens' Framework (Prelude.Maybe Prelude.Int)
framework_numberOfControls = Lens.lens (\Framework' {numberOfControls} -> numberOfControls) (\s@Framework' {} a -> s {numberOfControls = a} :: Framework)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
framework_frameworkArn :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkArn = Lens.lens (\Framework' {frameworkArn} -> frameworkArn) (\s@Framework' {} a -> s {frameworkArn = a} :: Framework)

-- | The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
framework_deploymentStatus :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_deploymentStatus = Lens.lens (\Framework' {deploymentStatus} -> deploymentStatus) (\s@Framework' {} a -> s {deploymentStatus = a} :: Framework)

-- | The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
framework_frameworkName :: Lens.Lens' Framework (Prelude.Maybe Prelude.Text)
framework_frameworkName = Lens.lens (\Framework' {frameworkName} -> frameworkName) (\s@Framework' {} a -> s {frameworkName = a} :: Framework)

instance Core.FromJSON Framework where
  parseJSON =
    Core.withObject
      "Framework"
      ( \x ->
          Framework'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "FrameworkDescription")
            Prelude.<*> (x Core..:? "NumberOfControls")
            Prelude.<*> (x Core..:? "FrameworkArn")
            Prelude.<*> (x Core..:? "DeploymentStatus")
            Prelude.<*> (x Core..:? "FrameworkName")
      )

instance Prelude.Hashable Framework where
  hashWithSalt _salt Framework' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` frameworkDescription
      `Prelude.hashWithSalt` numberOfControls
      `Prelude.hashWithSalt` frameworkArn
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` frameworkName

instance Prelude.NFData Framework where
  rnf Framework' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf frameworkDescription
      `Prelude.seq` Prelude.rnf numberOfControls
      `Prelude.seq` Prelude.rnf frameworkArn
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf frameworkName

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
-- Module      : Network.AWS.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.ProjectStatus

-- | A description of a Amazon Rekognition Custom Labels project.
--
-- /See:/ 'newProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { -- | The Unix timestamp for the date and time that the project was created.
    creationTimestamp :: Core.Maybe Core.POSIX,
    -- | The current status of the project.
    status :: Core.Maybe ProjectStatus,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProjectDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'projectDescription_creationTimestamp' - The Unix timestamp for the date and time that the project was created.
--
-- 'status', 'projectDescription_status' - The current status of the project.
--
-- 'projectArn', 'projectDescription_projectArn' - The Amazon Resource Name (ARN) of the project.
newProjectDescription ::
  ProjectDescription
newProjectDescription =
  ProjectDescription'
    { creationTimestamp =
        Core.Nothing,
      status = Core.Nothing,
      projectArn = Core.Nothing
    }

-- | The Unix timestamp for the date and time that the project was created.
projectDescription_creationTimestamp :: Lens.Lens' ProjectDescription (Core.Maybe Core.UTCTime)
projectDescription_creationTimestamp = Lens.lens (\ProjectDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectDescription' {} a -> s {creationTimestamp = a} :: ProjectDescription) Core.. Lens.mapping Core._Time

-- | The current status of the project.
projectDescription_status :: Lens.Lens' ProjectDescription (Core.Maybe ProjectStatus)
projectDescription_status = Lens.lens (\ProjectDescription' {status} -> status) (\s@ProjectDescription' {} a -> s {status = a} :: ProjectDescription)

-- | The Amazon Resource Name (ARN) of the project.
projectDescription_projectArn :: Lens.Lens' ProjectDescription (Core.Maybe Core.Text)
projectDescription_projectArn = Lens.lens (\ProjectDescription' {projectArn} -> projectArn) (\s@ProjectDescription' {} a -> s {projectArn = a} :: ProjectDescription)

instance Core.FromJSON ProjectDescription where
  parseJSON =
    Core.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Core.<$> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "ProjectArn")
      )

instance Core.Hashable ProjectDescription

instance Core.NFData ProjectDescription

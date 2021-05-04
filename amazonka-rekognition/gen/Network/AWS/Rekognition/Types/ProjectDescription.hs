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
-- Module      : Network.AWS.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.ProjectStatus

-- | A description of a Amazon Rekognition Custom Labels project.
--
-- /See:/ 'newProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { -- | The Unix timestamp for the date and time that the project was created.
    creationTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The current status of the project.
    status :: Prelude.Maybe ProjectStatus,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | The Unix timestamp for the date and time that the project was created.
projectDescription_creationTimestamp :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.UTCTime)
projectDescription_creationTimestamp = Lens.lens (\ProjectDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectDescription' {} a -> s {creationTimestamp = a} :: ProjectDescription) Prelude.. Lens.mapping Prelude._Time

-- | The current status of the project.
projectDescription_status :: Lens.Lens' ProjectDescription (Prelude.Maybe ProjectStatus)
projectDescription_status = Lens.lens (\ProjectDescription' {status} -> status) (\s@ProjectDescription' {} a -> s {status = a} :: ProjectDescription)

-- | The Amazon Resource Name (ARN) of the project.
projectDescription_projectArn :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectArn = Lens.lens (\ProjectDescription' {projectArn} -> projectArn) (\s@ProjectDescription' {} a -> s {projectArn = a} :: ProjectDescription)

instance Prelude.FromJSON ProjectDescription where
  parseJSON =
    Prelude.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Prelude.<$> (x Prelude..:? "CreationTimestamp")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "ProjectArn")
      )

instance Prelude.Hashable ProjectDescription

instance Prelude.NFData ProjectDescription

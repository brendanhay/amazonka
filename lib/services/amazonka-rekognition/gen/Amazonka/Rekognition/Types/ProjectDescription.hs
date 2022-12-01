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
-- Module      : Amazonka.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProjectDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DatasetMetadata
import Amazonka.Rekognition.Types.ProjectStatus

-- | A description of an Amazon Rekognition Custom Labels project. For more
-- information, see DescribeProjects.
--
-- /See:/ 'newProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { -- | Information about the training and test datasets in the project.
    datasets :: Prelude.Maybe [DatasetMetadata],
    -- | The current status of the project.
    status :: Prelude.Maybe ProjectStatus,
    -- | The Unix timestamp for the date and time that the project was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasets', 'projectDescription_datasets' - Information about the training and test datasets in the project.
--
-- 'status', 'projectDescription_status' - The current status of the project.
--
-- 'creationTimestamp', 'projectDescription_creationTimestamp' - The Unix timestamp for the date and time that the project was created.
--
-- 'projectArn', 'projectDescription_projectArn' - The Amazon Resource Name (ARN) of the project.
newProjectDescription ::
  ProjectDescription
newProjectDescription =
  ProjectDescription'
    { datasets = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | Information about the training and test datasets in the project.
projectDescription_datasets :: Lens.Lens' ProjectDescription (Prelude.Maybe [DatasetMetadata])
projectDescription_datasets = Lens.lens (\ProjectDescription' {datasets} -> datasets) (\s@ProjectDescription' {} a -> s {datasets = a} :: ProjectDescription) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the project.
projectDescription_status :: Lens.Lens' ProjectDescription (Prelude.Maybe ProjectStatus)
projectDescription_status = Lens.lens (\ProjectDescription' {status} -> status) (\s@ProjectDescription' {} a -> s {status = a} :: ProjectDescription)

-- | The Unix timestamp for the date and time that the project was created.
projectDescription_creationTimestamp :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.UTCTime)
projectDescription_creationTimestamp = Lens.lens (\ProjectDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectDescription' {} a -> s {creationTimestamp = a} :: ProjectDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the project.
projectDescription_projectArn :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectArn = Lens.lens (\ProjectDescription' {projectArn} -> projectArn) (\s@ProjectDescription' {} a -> s {projectArn = a} :: ProjectDescription)

instance Core.FromJSON ProjectDescription where
  parseJSON =
    Core.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Prelude.<$> (x Core..:? "Datasets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "ProjectArn")
      )

instance Prelude.Hashable ProjectDescription where
  hashWithSalt _salt ProjectDescription' {..} =
    _salt `Prelude.hashWithSalt` datasets
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ProjectDescription where
  rnf ProjectDescription' {..} =
    Prelude.rnf datasets
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf projectArn

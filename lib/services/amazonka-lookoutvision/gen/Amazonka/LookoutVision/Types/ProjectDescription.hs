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
-- Module      : Amazonka.LookoutVision.Types.ProjectDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ProjectDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.DatasetMetadata
import qualified Amazonka.Prelude as Prelude

-- | Describe an Amazon Lookout for Vision project. For more information, see
-- DescribeProject.
--
-- /See:/ 'newProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { -- | The unix timestamp for the date and time that the project was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of datasets in the project.
    datasets :: Prelude.Maybe [DatasetMetadata],
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text
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
-- 'creationTimestamp', 'projectDescription_creationTimestamp' - The unix timestamp for the date and time that the project was created.
--
-- 'datasets', 'projectDescription_datasets' - A list of datasets in the project.
--
-- 'projectArn', 'projectDescription_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectName', 'projectDescription_projectName' - The name of the project.
newProjectDescription ::
  ProjectDescription
newProjectDescription =
  ProjectDescription'
    { creationTimestamp =
        Prelude.Nothing,
      datasets = Prelude.Nothing,
      projectArn = Prelude.Nothing,
      projectName = Prelude.Nothing
    }

-- | The unix timestamp for the date and time that the project was created.
projectDescription_creationTimestamp :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.UTCTime)
projectDescription_creationTimestamp = Lens.lens (\ProjectDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectDescription' {} a -> s {creationTimestamp = a} :: ProjectDescription) Prelude.. Lens.mapping Data._Time

-- | A list of datasets in the project.
projectDescription_datasets :: Lens.Lens' ProjectDescription (Prelude.Maybe [DatasetMetadata])
projectDescription_datasets = Lens.lens (\ProjectDescription' {datasets} -> datasets) (\s@ProjectDescription' {} a -> s {datasets = a} :: ProjectDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the project.
projectDescription_projectArn :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectArn = Lens.lens (\ProjectDescription' {projectArn} -> projectArn) (\s@ProjectDescription' {} a -> s {projectArn = a} :: ProjectDescription)

-- | The name of the project.
projectDescription_projectName :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectName = Lens.lens (\ProjectDescription' {projectName} -> projectName) (\s@ProjectDescription' {} a -> s {projectName = a} :: ProjectDescription)

instance Data.FromJSON ProjectDescription where
  parseJSON =
    Data.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "Datasets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProjectArn")
            Prelude.<*> (x Data..:? "ProjectName")
      )

instance Prelude.Hashable ProjectDescription where
  hashWithSalt _salt ProjectDescription' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` datasets
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData ProjectDescription where
  rnf ProjectDescription' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf datasets
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectName

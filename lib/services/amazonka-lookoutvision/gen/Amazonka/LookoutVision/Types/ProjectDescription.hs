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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | A list of datasets in the project.
    datasets :: Prelude.Maybe [DatasetMetadata],
    -- | The unix timestamp for the date and time that the project was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
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
-- 'datasets', 'projectDescription_datasets' - A list of datasets in the project.
--
-- 'creationTimestamp', 'projectDescription_creationTimestamp' - The unix timestamp for the date and time that the project was created.
--
-- 'projectName', 'projectDescription_projectName' - The name of the project.
--
-- 'projectArn', 'projectDescription_projectArn' - The Amazon Resource Name (ARN) of the project.
newProjectDescription ::
  ProjectDescription
newProjectDescription =
  ProjectDescription'
    { datasets = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      projectName = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | A list of datasets in the project.
projectDescription_datasets :: Lens.Lens' ProjectDescription (Prelude.Maybe [DatasetMetadata])
projectDescription_datasets = Lens.lens (\ProjectDescription' {datasets} -> datasets) (\s@ProjectDescription' {} a -> s {datasets = a} :: ProjectDescription) Prelude.. Lens.mapping Lens.coerced

-- | The unix timestamp for the date and time that the project was created.
projectDescription_creationTimestamp :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.UTCTime)
projectDescription_creationTimestamp = Lens.lens (\ProjectDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectDescription' {} a -> s {creationTimestamp = a} :: ProjectDescription) Prelude.. Lens.mapping Data._Time

-- | The name of the project.
projectDescription_projectName :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectName = Lens.lens (\ProjectDescription' {projectName} -> projectName) (\s@ProjectDescription' {} a -> s {projectName = a} :: ProjectDescription)

-- | The Amazon Resource Name (ARN) of the project.
projectDescription_projectArn :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_projectArn = Lens.lens (\ProjectDescription' {projectArn} -> projectArn) (\s@ProjectDescription' {} a -> s {projectArn = a} :: ProjectDescription)

instance Data.FromJSON ProjectDescription where
  parseJSON =
    Data.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Prelude.<$> (x Data..:? "Datasets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "ProjectArn")
      )

instance Prelude.Hashable ProjectDescription where
  hashWithSalt _salt ProjectDescription' {..} =
    _salt `Prelude.hashWithSalt` datasets
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ProjectDescription where
  rnf ProjectDescription' {..} =
    Prelude.rnf datasets
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf projectArn

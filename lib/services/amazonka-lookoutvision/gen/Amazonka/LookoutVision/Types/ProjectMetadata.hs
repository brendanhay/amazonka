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
-- Module      : Amazonka.LookoutVision.Types.ProjectMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ProjectMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about an Amazon Lookout for Vision project.
--
-- /See:/ 'newProjectMetadata' smart constructor.
data ProjectMetadata = ProjectMetadata'
  { -- | The unix timestamp for the date and time that the project was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'projectMetadata_creationTimestamp' - The unix timestamp for the date and time that the project was created.
--
-- 'projectName', 'projectMetadata_projectName' - The name of the project.
--
-- 'projectArn', 'projectMetadata_projectArn' - The Amazon Resource Name (ARN) of the project.
newProjectMetadata ::
  ProjectMetadata
newProjectMetadata =
  ProjectMetadata'
    { creationTimestamp =
        Prelude.Nothing,
      projectName = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | The unix timestamp for the date and time that the project was created.
projectMetadata_creationTimestamp :: Lens.Lens' ProjectMetadata (Prelude.Maybe Prelude.UTCTime)
projectMetadata_creationTimestamp = Lens.lens (\ProjectMetadata' {creationTimestamp} -> creationTimestamp) (\s@ProjectMetadata' {} a -> s {creationTimestamp = a} :: ProjectMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the project.
projectMetadata_projectName :: Lens.Lens' ProjectMetadata (Prelude.Maybe Prelude.Text)
projectMetadata_projectName = Lens.lens (\ProjectMetadata' {projectName} -> projectName) (\s@ProjectMetadata' {} a -> s {projectName = a} :: ProjectMetadata)

-- | The Amazon Resource Name (ARN) of the project.
projectMetadata_projectArn :: Lens.Lens' ProjectMetadata (Prelude.Maybe Prelude.Text)
projectMetadata_projectArn = Lens.lens (\ProjectMetadata' {projectArn} -> projectArn) (\s@ProjectMetadata' {} a -> s {projectArn = a} :: ProjectMetadata)

instance Data.FromJSON ProjectMetadata where
  parseJSON =
    Data.withObject
      "ProjectMetadata"
      ( \x ->
          ProjectMetadata'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "ProjectArn")
      )

instance Prelude.Hashable ProjectMetadata where
  hashWithSalt _salt ProjectMetadata' {..} =
    _salt `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ProjectMetadata where
  rnf ProjectMetadata' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf projectArn

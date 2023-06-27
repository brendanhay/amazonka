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
-- Module      : Amazonka.DataBrew.Types.Project
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.Sample
import qualified Amazonka.Prelude as Prelude

-- | Represents all of the attributes of a DataBrew project.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The ID of the Amazon Web Services account that owns the project.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the project was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who crated the project.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The dataset that the project is to act upon.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who last modified the
    -- project.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The last modification date and time for the project.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the project was opened.
    openDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user that opened the project for
    -- use.
    openedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the project.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that will be assumed for this
    -- project.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The sample size and sampling type to apply to the data. If this
    -- parameter isn\'t specified, then the sample consists of the first 500
    -- rows from the dataset.
    sample :: Prelude.Maybe Sample,
    -- | Metadata tags that have been applied to the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of a project.
    name :: Prelude.Text,
    -- | The name of a recipe that will be developed during a project session.
    recipeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'project_accountId' - The ID of the Amazon Web Services account that owns the project.
--
-- 'createDate', 'project_createDate' - The date and time that the project was created.
--
-- 'createdBy', 'project_createdBy' - The Amazon Resource Name (ARN) of the user who crated the project.
--
-- 'datasetName', 'project_datasetName' - The dataset that the project is to act upon.
--
-- 'lastModifiedBy', 'project_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the
-- project.
--
-- 'lastModifiedDate', 'project_lastModifiedDate' - The last modification date and time for the project.
--
-- 'openDate', 'project_openDate' - The date and time when the project was opened.
--
-- 'openedBy', 'project_openedBy' - The Amazon Resource Name (ARN) of the user that opened the project for
-- use.
--
-- 'resourceArn', 'project_resourceArn' - The Amazon Resource Name (ARN) for the project.
--
-- 'roleArn', 'project_roleArn' - The Amazon Resource Name (ARN) of the role that will be assumed for this
-- project.
--
-- 'sample', 'project_sample' - The sample size and sampling type to apply to the data. If this
-- parameter isn\'t specified, then the sample consists of the first 500
-- rows from the dataset.
--
-- 'tags', 'project_tags' - Metadata tags that have been applied to the project.
--
-- 'name', 'project_name' - The unique name of a project.
--
-- 'recipeName', 'project_recipeName' - The name of a recipe that will be developed during a project session.
newProject ::
  -- | 'name'
  Prelude.Text ->
  -- | 'recipeName'
  Prelude.Text ->
  Project
newProject pName_ pRecipeName_ =
  Project'
    { accountId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      openDate = Prelude.Nothing,
      openedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sample = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      recipeName = pRecipeName_
    }

-- | The ID of the Amazon Web Services account that owns the project.
project_accountId :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_accountId = Lens.lens (\Project' {accountId} -> accountId) (\s@Project' {} a -> s {accountId = a} :: Project)

-- | The date and time that the project was created.
project_createDate :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_createDate = Lens.lens (\Project' {createDate} -> createDate) (\s@Project' {} a -> s {createDate = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the user who crated the project.
project_createdBy :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_createdBy = Lens.lens (\Project' {createdBy} -> createdBy) (\s@Project' {} a -> s {createdBy = a} :: Project)

-- | The dataset that the project is to act upon.
project_datasetName :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_datasetName = Lens.lens (\Project' {datasetName} -> datasetName) (\s@Project' {} a -> s {datasetName = a} :: Project)

-- | The Amazon Resource Name (ARN) of the user who last modified the
-- project.
project_lastModifiedBy :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_lastModifiedBy = Lens.lens (\Project' {lastModifiedBy} -> lastModifiedBy) (\s@Project' {} a -> s {lastModifiedBy = a} :: Project)

-- | The last modification date and time for the project.
project_lastModifiedDate :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_lastModifiedDate = Lens.lens (\Project' {lastModifiedDate} -> lastModifiedDate) (\s@Project' {} a -> s {lastModifiedDate = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | The date and time when the project was opened.
project_openDate :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_openDate = Lens.lens (\Project' {openDate} -> openDate) (\s@Project' {} a -> s {openDate = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the user that opened the project for
-- use.
project_openedBy :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_openedBy = Lens.lens (\Project' {openedBy} -> openedBy) (\s@Project' {} a -> s {openedBy = a} :: Project)

-- | The Amazon Resource Name (ARN) for the project.
project_resourceArn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_resourceArn = Lens.lens (\Project' {resourceArn} -> resourceArn) (\s@Project' {} a -> s {resourceArn = a} :: Project)

-- | The Amazon Resource Name (ARN) of the role that will be assumed for this
-- project.
project_roleArn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_roleArn = Lens.lens (\Project' {roleArn} -> roleArn) (\s@Project' {} a -> s {roleArn = a} :: Project)

-- | The sample size and sampling type to apply to the data. If this
-- parameter isn\'t specified, then the sample consists of the first 500
-- rows from the dataset.
project_sample :: Lens.Lens' Project (Prelude.Maybe Sample)
project_sample = Lens.lens (\Project' {sample} -> sample) (\s@Project' {} a -> s {sample = a} :: Project)

-- | Metadata tags that have been applied to the project.
project_tags :: Lens.Lens' Project (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
project_tags = Lens.lens (\Project' {tags} -> tags) (\s@Project' {} a -> s {tags = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of a project.
project_name :: Lens.Lens' Project Prelude.Text
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | The name of a recipe that will be developed during a project session.
project_recipeName :: Lens.Lens' Project Prelude.Text
project_recipeName = Lens.lens (\Project' {recipeName} -> recipeName) (\s@Project' {} a -> s {recipeName = a} :: Project)

instance Data.FromJSON Project where
  parseJSON =
    Data.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "OpenDate")
            Prelude.<*> (x Data..:? "OpenedBy")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Sample")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "RecipeName")
      )

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` openDate
      `Prelude.hashWithSalt` openedBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sample
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recipeName

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf openDate
      `Prelude.seq` Prelude.rnf openedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sample
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recipeName

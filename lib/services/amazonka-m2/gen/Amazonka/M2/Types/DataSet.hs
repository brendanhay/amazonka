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
-- Module      : Amazonka.M2.Types.DataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.DatasetOrgAttributes
import Amazonka.M2.Types.RecordLength
import qualified Amazonka.Prelude as Prelude

-- | Defines a data set.
--
-- /See:/ 'newDataSet' smart constructor.
data DataSet = DataSet'
  { -- | The relative location of the data set in the database or file system.
    relativePath :: Prelude.Maybe Prelude.Text,
    -- | The storage type of the data set: database or file system. For Micro
    -- Focus, database corresponds to datastore and file system corresponds to
    -- EFS\/FSX. For Blu Age, there is no support of file system and database
    -- corresponds to Blusam.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The logical identifier for a specific data set (in mainframe format).
    datasetName :: Prelude.Text,
    -- | The type of dataset. The only supported value is VSAM.
    datasetOrg :: DatasetOrgAttributes,
    -- | The length of a record.
    recordLength :: RecordLength
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relativePath', 'dataSet_relativePath' - The relative location of the data set in the database or file system.
--
-- 'storageType', 'dataSet_storageType' - The storage type of the data set: database or file system. For Micro
-- Focus, database corresponds to datastore and file system corresponds to
-- EFS\/FSX. For Blu Age, there is no support of file system and database
-- corresponds to Blusam.
--
-- 'datasetName', 'dataSet_datasetName' - The logical identifier for a specific data set (in mainframe format).
--
-- 'datasetOrg', 'dataSet_datasetOrg' - The type of dataset. The only supported value is VSAM.
--
-- 'recordLength', 'dataSet_recordLength' - The length of a record.
newDataSet ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'datasetOrg'
  DatasetOrgAttributes ->
  -- | 'recordLength'
  RecordLength ->
  DataSet
newDataSet pDatasetName_ pDatasetOrg_ pRecordLength_ =
  DataSet'
    { relativePath = Prelude.Nothing,
      storageType = Prelude.Nothing,
      datasetName = pDatasetName_,
      datasetOrg = pDatasetOrg_,
      recordLength = pRecordLength_
    }

-- | The relative location of the data set in the database or file system.
dataSet_relativePath :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_relativePath = Lens.lens (\DataSet' {relativePath} -> relativePath) (\s@DataSet' {} a -> s {relativePath = a} :: DataSet)

-- | The storage type of the data set: database or file system. For Micro
-- Focus, database corresponds to datastore and file system corresponds to
-- EFS\/FSX. For Blu Age, there is no support of file system and database
-- corresponds to Blusam.
dataSet_storageType :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_storageType = Lens.lens (\DataSet' {storageType} -> storageType) (\s@DataSet' {} a -> s {storageType = a} :: DataSet)

-- | The logical identifier for a specific data set (in mainframe format).
dataSet_datasetName :: Lens.Lens' DataSet Prelude.Text
dataSet_datasetName = Lens.lens (\DataSet' {datasetName} -> datasetName) (\s@DataSet' {} a -> s {datasetName = a} :: DataSet)

-- | The type of dataset. The only supported value is VSAM.
dataSet_datasetOrg :: Lens.Lens' DataSet DatasetOrgAttributes
dataSet_datasetOrg = Lens.lens (\DataSet' {datasetOrg} -> datasetOrg) (\s@DataSet' {} a -> s {datasetOrg = a} :: DataSet)

-- | The length of a record.
dataSet_recordLength :: Lens.Lens' DataSet RecordLength
dataSet_recordLength = Lens.lens (\DataSet' {recordLength} -> recordLength) (\s@DataSet' {} a -> s {recordLength = a} :: DataSet)

instance Prelude.Hashable DataSet where
  hashWithSalt _salt DataSet' {..} =
    _salt `Prelude.hashWithSalt` relativePath
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` datasetOrg
      `Prelude.hashWithSalt` recordLength

instance Prelude.NFData DataSet where
  rnf DataSet' {..} =
    Prelude.rnf relativePath
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf datasetOrg
      `Prelude.seq` Prelude.rnf recordLength

instance Data.ToJSON DataSet where
  toJSON DataSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("relativePath" Data..=) Prelude.<$> relativePath,
            ("storageType" Data..=) Prelude.<$> storageType,
            Prelude.Just ("datasetName" Data..= datasetName),
            Prelude.Just ("datasetOrg" Data..= datasetOrg),
            Prelude.Just ("recordLength" Data..= recordLength)
          ]
      )

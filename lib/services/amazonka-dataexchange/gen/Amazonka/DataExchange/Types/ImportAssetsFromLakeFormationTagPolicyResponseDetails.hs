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
-- Module      : Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyResponseDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.DatabaseLFTagPolicyAndPermissions
import Amazonka.DataExchange.Types.TableLFTagPolicyAndPermissions
import qualified Amazonka.Prelude as Prelude

-- | Details from an import AWS Lake Formation tag policy job response.
--
-- /See:/ 'newImportAssetsFromLakeFormationTagPolicyResponseDetails' smart constructor.
data ImportAssetsFromLakeFormationTagPolicyResponseDetails = ImportAssetsFromLakeFormationTagPolicyResponseDetails'
  { -- | A structure for the database object.
    database :: Prelude.Maybe DatabaseLFTagPolicyAndPermissions,
    -- | A structure for the table object.
    table :: Prelude.Maybe TableLFTagPolicyAndPermissions,
    -- | The identifier for the AWS Glue Data Catalog.
    catalogId :: Prelude.Text,
    -- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
    -- grant and revoke permissions to AWS Lake Formation data permissions.
    roleArn :: Prelude.Text,
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import job.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetsFromLakeFormationTagPolicyResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'importAssetsFromLakeFormationTagPolicyResponseDetails_database' - A structure for the database object.
--
-- 'table', 'importAssetsFromLakeFormationTagPolicyResponseDetails_table' - A structure for the table object.
--
-- 'catalogId', 'importAssetsFromLakeFormationTagPolicyResponseDetails_catalogId' - The identifier for the AWS Glue Data Catalog.
--
-- 'roleArn', 'importAssetsFromLakeFormationTagPolicyResponseDetails_roleArn' - The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions to AWS Lake Formation data permissions.
--
-- 'dataSetId', 'importAssetsFromLakeFormationTagPolicyResponseDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'revisionId', 'importAssetsFromLakeFormationTagPolicyResponseDetails_revisionId' - The unique identifier for the revision associated with this import job.
newImportAssetsFromLakeFormationTagPolicyResponseDetails ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetsFromLakeFormationTagPolicyResponseDetails
newImportAssetsFromLakeFormationTagPolicyResponseDetails
  pCatalogId_
  pRoleArn_
  pDataSetId_
  pRevisionId_ =
    ImportAssetsFromLakeFormationTagPolicyResponseDetails'
      { database =
          Prelude.Nothing,
        table =
          Prelude.Nothing,
        catalogId =
          pCatalogId_,
        roleArn = pRoleArn_,
        dataSetId =
          pDataSetId_,
        revisionId =
          pRevisionId_
      }

-- | A structure for the database object.
importAssetsFromLakeFormationTagPolicyResponseDetails_database :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails (Prelude.Maybe DatabaseLFTagPolicyAndPermissions)
importAssetsFromLakeFormationTagPolicyResponseDetails_database = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {database} -> database) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {database = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

-- | A structure for the table object.
importAssetsFromLakeFormationTagPolicyResponseDetails_table :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails (Prelude.Maybe TableLFTagPolicyAndPermissions)
importAssetsFromLakeFormationTagPolicyResponseDetails_table = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {table} -> table) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {table = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

-- | The identifier for the AWS Glue Data Catalog.
importAssetsFromLakeFormationTagPolicyResponseDetails_catalogId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyResponseDetails_catalogId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {catalogId} -> catalogId) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {catalogId = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

-- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions to AWS Lake Formation data permissions.
importAssetsFromLakeFormationTagPolicyResponseDetails_roleArn :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyResponseDetails_roleArn = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {roleArn} -> roleArn) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {roleArn = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

-- | The unique identifier for the data set associated with this import job.
importAssetsFromLakeFormationTagPolicyResponseDetails_dataSetId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyResponseDetails_dataSetId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {dataSetId} -> dataSetId) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {dataSetId = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

-- | The unique identifier for the revision associated with this import job.
importAssetsFromLakeFormationTagPolicyResponseDetails_revisionId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyResponseDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyResponseDetails_revisionId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyResponseDetails' {revisionId} -> revisionId) (\s@ImportAssetsFromLakeFormationTagPolicyResponseDetails' {} a -> s {revisionId = a} :: ImportAssetsFromLakeFormationTagPolicyResponseDetails)

instance
  Data.FromJSON
    ImportAssetsFromLakeFormationTagPolicyResponseDetails
  where
  parseJSON =
    Data.withObject
      "ImportAssetsFromLakeFormationTagPolicyResponseDetails"
      ( \x ->
          ImportAssetsFromLakeFormationTagPolicyResponseDetails'
            Prelude.<$> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "Table")
            Prelude.<*> (x Data..: "CatalogId")
            Prelude.<*> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "RevisionId")
      )

instance
  Prelude.Hashable
    ImportAssetsFromLakeFormationTagPolicyResponseDetails
  where
  hashWithSalt
    _salt
    ImportAssetsFromLakeFormationTagPolicyResponseDetails' {..} =
      _salt
        `Prelude.hashWithSalt` database
        `Prelude.hashWithSalt` table
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ImportAssetsFromLakeFormationTagPolicyResponseDetails
  where
  rnf
    ImportAssetsFromLakeFormationTagPolicyResponseDetails' {..} =
      Prelude.rnf database
        `Prelude.seq` Prelude.rnf table
        `Prelude.seq` Prelude.rnf catalogId
        `Prelude.seq` Prelude.rnf roleArn
        `Prelude.seq` Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf revisionId

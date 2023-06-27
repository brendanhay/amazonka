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
-- Module      : Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyRequestDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyRequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.DatabaseLFTagPolicyAndPermissions
import Amazonka.DataExchange.Types.TableLFTagPolicyAndPermissions
import qualified Amazonka.Prelude as Prelude

-- | Details about the assets imported from an AWS Lake Formation tag policy
-- request.
--
-- /See:/ 'newImportAssetsFromLakeFormationTagPolicyRequestDetails' smart constructor.
data ImportAssetsFromLakeFormationTagPolicyRequestDetails = ImportAssetsFromLakeFormationTagPolicyRequestDetails'
  { -- | A structure for the database object.
    database :: Prelude.Maybe DatabaseLFTagPolicyAndPermissions,
    -- | A structure for the table object.
    table :: Prelude.Maybe TableLFTagPolicyAndPermissions,
    -- | The identifier for the AWS Glue Data Catalog.
    catalogId :: Prelude.Text,
    -- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
    -- grant and revoke permissions of subscribers to AWS Lake Formation data
    -- permissions.
    roleArn :: Prelude.Text,
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import job.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetsFromLakeFormationTagPolicyRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'importAssetsFromLakeFormationTagPolicyRequestDetails_database' - A structure for the database object.
--
-- 'table', 'importAssetsFromLakeFormationTagPolicyRequestDetails_table' - A structure for the table object.
--
-- 'catalogId', 'importAssetsFromLakeFormationTagPolicyRequestDetails_catalogId' - The identifier for the AWS Glue Data Catalog.
--
-- 'roleArn', 'importAssetsFromLakeFormationTagPolicyRequestDetails_roleArn' - The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions of subscribers to AWS Lake Formation data
-- permissions.
--
-- 'dataSetId', 'importAssetsFromLakeFormationTagPolicyRequestDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'revisionId', 'importAssetsFromLakeFormationTagPolicyRequestDetails_revisionId' - The unique identifier for the revision associated with this import job.
newImportAssetsFromLakeFormationTagPolicyRequestDetails ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetsFromLakeFormationTagPolicyRequestDetails
newImportAssetsFromLakeFormationTagPolicyRequestDetails
  pCatalogId_
  pRoleArn_
  pDataSetId_
  pRevisionId_ =
    ImportAssetsFromLakeFormationTagPolicyRequestDetails'
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
importAssetsFromLakeFormationTagPolicyRequestDetails_database :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails (Prelude.Maybe DatabaseLFTagPolicyAndPermissions)
importAssetsFromLakeFormationTagPolicyRequestDetails_database = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {database} -> database) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {database = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

-- | A structure for the table object.
importAssetsFromLakeFormationTagPolicyRequestDetails_table :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails (Prelude.Maybe TableLFTagPolicyAndPermissions)
importAssetsFromLakeFormationTagPolicyRequestDetails_table = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {table} -> table) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {table = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

-- | The identifier for the AWS Glue Data Catalog.
importAssetsFromLakeFormationTagPolicyRequestDetails_catalogId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyRequestDetails_catalogId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {catalogId} -> catalogId) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {catalogId = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

-- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions of subscribers to AWS Lake Formation data
-- permissions.
importAssetsFromLakeFormationTagPolicyRequestDetails_roleArn :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyRequestDetails_roleArn = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {roleArn} -> roleArn) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {roleArn = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

-- | The unique identifier for the data set associated with this import job.
importAssetsFromLakeFormationTagPolicyRequestDetails_dataSetId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyRequestDetails_dataSetId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {dataSetId} -> dataSetId) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {dataSetId = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

-- | The unique identifier for the revision associated with this import job.
importAssetsFromLakeFormationTagPolicyRequestDetails_revisionId :: Lens.Lens' ImportAssetsFromLakeFormationTagPolicyRequestDetails Prelude.Text
importAssetsFromLakeFormationTagPolicyRequestDetails_revisionId = Lens.lens (\ImportAssetsFromLakeFormationTagPolicyRequestDetails' {revisionId} -> revisionId) (\s@ImportAssetsFromLakeFormationTagPolicyRequestDetails' {} a -> s {revisionId = a} :: ImportAssetsFromLakeFormationTagPolicyRequestDetails)

instance
  Prelude.Hashable
    ImportAssetsFromLakeFormationTagPolicyRequestDetails
  where
  hashWithSalt
    _salt
    ImportAssetsFromLakeFormationTagPolicyRequestDetails' {..} =
      _salt
        `Prelude.hashWithSalt` database
        `Prelude.hashWithSalt` table
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ImportAssetsFromLakeFormationTagPolicyRequestDetails
  where
  rnf
    ImportAssetsFromLakeFormationTagPolicyRequestDetails' {..} =
      Prelude.rnf database
        `Prelude.seq` Prelude.rnf table
        `Prelude.seq` Prelude.rnf catalogId
        `Prelude.seq` Prelude.rnf roleArn
        `Prelude.seq` Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf revisionId

instance
  Data.ToJSON
    ImportAssetsFromLakeFormationTagPolicyRequestDetails
  where
  toJSON
    ImportAssetsFromLakeFormationTagPolicyRequestDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Database" Data..=) Prelude.<$> database,
              ("Table" Data..=) Prelude.<$> table,
              Prelude.Just ("CatalogId" Data..= catalogId),
              Prelude.Just ("RoleArn" Data..= roleArn),
              Prelude.Just ("DataSetId" Data..= dataSetId),
              Prelude.Just ("RevisionId" Data..= revisionId)
            ]
        )

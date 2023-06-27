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
-- Module      : Amazonka.CleanRooms.Types.Schema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.Schema where

import Amazonka.CleanRooms.Types.AnalysisMethod
import Amazonka.CleanRooms.Types.AnalysisRuleType
import Amazonka.CleanRooms.Types.Column
import Amazonka.CleanRooms.Types.SchemaType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A schema is a relation within a collaboration.
--
-- /See:/ 'newSchema' smart constructor.
data Schema = Schema'
  { -- | The analysis method for the schema. The only valid value is currently
    -- DIRECT_QUERY.
    analysisMethod :: Prelude.Maybe AnalysisMethod,
    -- | The columns for the relation this schema represents.
    columns :: [Column],
    -- | The partition keys for the dataset underlying this schema.
    partitionKeys :: [Column],
    -- | The analysis rule types associated with the schema. Valued values are
    -- LIST and AGGREGATION. Currently, only one entry is present.
    analysisRuleTypes :: [AnalysisRuleType],
    -- | The unique account ID for the AWS account that owns the schema.
    creatorAccountId :: Prelude.Text,
    -- | A name for the schema. The schema relation is referred to by this name
    -- when queried by a protected query.
    name :: Prelude.Text,
    -- | The unique ID for the collaboration that the schema belongs to.
    collaborationId :: Prelude.Text,
    -- | The unique ARN for the collaboration that the schema belongs to.
    collaborationArn :: Prelude.Text,
    -- | A description for the schema.
    description :: Prelude.Text,
    -- | The time the schema was created.
    createTime :: Data.POSIX,
    -- | The time the schema was last updated.
    updateTime :: Data.POSIX,
    -- | The type of schema. The only valid value is currently \`TABLE\`.
    type' :: SchemaType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisMethod', 'schema_analysisMethod' - The analysis method for the schema. The only valid value is currently
-- DIRECT_QUERY.
--
-- 'columns', 'schema_columns' - The columns for the relation this schema represents.
--
-- 'partitionKeys', 'schema_partitionKeys' - The partition keys for the dataset underlying this schema.
--
-- 'analysisRuleTypes', 'schema_analysisRuleTypes' - The analysis rule types associated with the schema. Valued values are
-- LIST and AGGREGATION. Currently, only one entry is present.
--
-- 'creatorAccountId', 'schema_creatorAccountId' - The unique account ID for the AWS account that owns the schema.
--
-- 'name', 'schema_name' - A name for the schema. The schema relation is referred to by this name
-- when queried by a protected query.
--
-- 'collaborationId', 'schema_collaborationId' - The unique ID for the collaboration that the schema belongs to.
--
-- 'collaborationArn', 'schema_collaborationArn' - The unique ARN for the collaboration that the schema belongs to.
--
-- 'description', 'schema_description' - A description for the schema.
--
-- 'createTime', 'schema_createTime' - The time the schema was created.
--
-- 'updateTime', 'schema_updateTime' - The time the schema was last updated.
--
-- 'type'', 'schema_type' - The type of schema. The only valid value is currently \`TABLE\`.
newSchema ::
  -- | 'creatorAccountId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'collaborationId'
  Prelude.Text ->
  -- | 'collaborationArn'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'type''
  SchemaType ->
  Schema
newSchema
  pCreatorAccountId_
  pName_
  pCollaborationId_
  pCollaborationArn_
  pDescription_
  pCreateTime_
  pUpdateTime_
  pType_ =
    Schema'
      { analysisMethod = Prelude.Nothing,
        columns = Prelude.mempty,
        partitionKeys = Prelude.mempty,
        analysisRuleTypes = Prelude.mempty,
        creatorAccountId = pCreatorAccountId_,
        name = pName_,
        collaborationId = pCollaborationId_,
        collaborationArn = pCollaborationArn_,
        description = pDescription_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        type' = pType_
      }

-- | The analysis method for the schema. The only valid value is currently
-- DIRECT_QUERY.
schema_analysisMethod :: Lens.Lens' Schema (Prelude.Maybe AnalysisMethod)
schema_analysisMethod = Lens.lens (\Schema' {analysisMethod} -> analysisMethod) (\s@Schema' {} a -> s {analysisMethod = a} :: Schema)

-- | The columns for the relation this schema represents.
schema_columns :: Lens.Lens' Schema [Column]
schema_columns = Lens.lens (\Schema' {columns} -> columns) (\s@Schema' {} a -> s {columns = a} :: Schema) Prelude.. Lens.coerced

-- | The partition keys for the dataset underlying this schema.
schema_partitionKeys :: Lens.Lens' Schema [Column]
schema_partitionKeys = Lens.lens (\Schema' {partitionKeys} -> partitionKeys) (\s@Schema' {} a -> s {partitionKeys = a} :: Schema) Prelude.. Lens.coerced

-- | The analysis rule types associated with the schema. Valued values are
-- LIST and AGGREGATION. Currently, only one entry is present.
schema_analysisRuleTypes :: Lens.Lens' Schema [AnalysisRuleType]
schema_analysisRuleTypes = Lens.lens (\Schema' {analysisRuleTypes} -> analysisRuleTypes) (\s@Schema' {} a -> s {analysisRuleTypes = a} :: Schema) Prelude.. Lens.coerced

-- | The unique account ID for the AWS account that owns the schema.
schema_creatorAccountId :: Lens.Lens' Schema Prelude.Text
schema_creatorAccountId = Lens.lens (\Schema' {creatorAccountId} -> creatorAccountId) (\s@Schema' {} a -> s {creatorAccountId = a} :: Schema)

-- | A name for the schema. The schema relation is referred to by this name
-- when queried by a protected query.
schema_name :: Lens.Lens' Schema Prelude.Text
schema_name = Lens.lens (\Schema' {name} -> name) (\s@Schema' {} a -> s {name = a} :: Schema)

-- | The unique ID for the collaboration that the schema belongs to.
schema_collaborationId :: Lens.Lens' Schema Prelude.Text
schema_collaborationId = Lens.lens (\Schema' {collaborationId} -> collaborationId) (\s@Schema' {} a -> s {collaborationId = a} :: Schema)

-- | The unique ARN for the collaboration that the schema belongs to.
schema_collaborationArn :: Lens.Lens' Schema Prelude.Text
schema_collaborationArn = Lens.lens (\Schema' {collaborationArn} -> collaborationArn) (\s@Schema' {} a -> s {collaborationArn = a} :: Schema)

-- | A description for the schema.
schema_description :: Lens.Lens' Schema Prelude.Text
schema_description = Lens.lens (\Schema' {description} -> description) (\s@Schema' {} a -> s {description = a} :: Schema)

-- | The time the schema was created.
schema_createTime :: Lens.Lens' Schema Prelude.UTCTime
schema_createTime = Lens.lens (\Schema' {createTime} -> createTime) (\s@Schema' {} a -> s {createTime = a} :: Schema) Prelude.. Data._Time

-- | The time the schema was last updated.
schema_updateTime :: Lens.Lens' Schema Prelude.UTCTime
schema_updateTime = Lens.lens (\Schema' {updateTime} -> updateTime) (\s@Schema' {} a -> s {updateTime = a} :: Schema) Prelude.. Data._Time

-- | The type of schema. The only valid value is currently \`TABLE\`.
schema_type :: Lens.Lens' Schema SchemaType
schema_type = Lens.lens (\Schema' {type'} -> type') (\s@Schema' {} a -> s {type' = a} :: Schema)

instance Data.FromJSON Schema where
  parseJSON =
    Data.withObject
      "Schema"
      ( \x ->
          Schema'
            Prelude.<$> (x Data..:? "analysisMethod")
            Prelude.<*> (x Data..:? "columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "partitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "analysisRuleTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "creatorAccountId")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "collaborationId")
            Prelude.<*> (x Data..: "collaborationArn")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Schema where
  hashWithSalt _salt Schema' {..} =
    _salt
      `Prelude.hashWithSalt` analysisMethod
      `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` analysisRuleTypes
      `Prelude.hashWithSalt` creatorAccountId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` collaborationId
      `Prelude.hashWithSalt` collaborationArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Schema where
  rnf Schema' {..} =
    Prelude.rnf analysisMethod
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf analysisRuleTypes
      `Prelude.seq` Prelude.rnf creatorAccountId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf collaborationId
      `Prelude.seq` Prelude.rnf collaborationArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf type'

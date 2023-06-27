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
-- Module      : Amazonka.CleanRooms.Types.SchemaSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.SchemaSummary where

import Amazonka.CleanRooms.Types.AnalysisMethod
import Amazonka.CleanRooms.Types.AnalysisRuleType
import Amazonka.CleanRooms.Types.SchemaType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The schema summary for the objects listed by the request.
--
-- /See:/ 'newSchemaSummary' smart constructor.
data SchemaSummary = SchemaSummary'
  { -- | The analysis method for the associated schema. The only valid value is
    -- currently \`DIRECT_QUERY\`.
    analysisMethod :: Prelude.Maybe AnalysisMethod,
    -- | The name for the schema object.
    name :: Prelude.Text,
    -- | The type of schema object. The only valid schema type is currently
    -- \`TABLE\`.
    type' :: SchemaType,
    -- | The unique account ID for the AWS account that owns the schema.
    creatorAccountId :: Prelude.Text,
    -- | The time the schema object was created.
    createTime :: Data.POSIX,
    -- | The time the schema object was last updated.
    updateTime :: Data.POSIX,
    -- | The unique ID for the collaboration that the schema belongs to.
    collaborationId :: Prelude.Text,
    -- | The unique ARN for the collaboration that the schema belongs to.
    collaborationArn :: Prelude.Text,
    -- | The types of analysis rules that are associated with this schema object.
    analysisRuleTypes :: [AnalysisRuleType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisMethod', 'schemaSummary_analysisMethod' - The analysis method for the associated schema. The only valid value is
-- currently \`DIRECT_QUERY\`.
--
-- 'name', 'schemaSummary_name' - The name for the schema object.
--
-- 'type'', 'schemaSummary_type' - The type of schema object. The only valid schema type is currently
-- \`TABLE\`.
--
-- 'creatorAccountId', 'schemaSummary_creatorAccountId' - The unique account ID for the AWS account that owns the schema.
--
-- 'createTime', 'schemaSummary_createTime' - The time the schema object was created.
--
-- 'updateTime', 'schemaSummary_updateTime' - The time the schema object was last updated.
--
-- 'collaborationId', 'schemaSummary_collaborationId' - The unique ID for the collaboration that the schema belongs to.
--
-- 'collaborationArn', 'schemaSummary_collaborationArn' - The unique ARN for the collaboration that the schema belongs to.
--
-- 'analysisRuleTypes', 'schemaSummary_analysisRuleTypes' - The types of analysis rules that are associated with this schema object.
newSchemaSummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  SchemaType ->
  -- | 'creatorAccountId'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'collaborationId'
  Prelude.Text ->
  -- | 'collaborationArn'
  Prelude.Text ->
  SchemaSummary
newSchemaSummary
  pName_
  pType_
  pCreatorAccountId_
  pCreateTime_
  pUpdateTime_
  pCollaborationId_
  pCollaborationArn_ =
    SchemaSummary'
      { analysisMethod = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        creatorAccountId = pCreatorAccountId_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        collaborationId = pCollaborationId_,
        collaborationArn = pCollaborationArn_,
        analysisRuleTypes = Prelude.mempty
      }

-- | The analysis method for the associated schema. The only valid value is
-- currently \`DIRECT_QUERY\`.
schemaSummary_analysisMethod :: Lens.Lens' SchemaSummary (Prelude.Maybe AnalysisMethod)
schemaSummary_analysisMethod = Lens.lens (\SchemaSummary' {analysisMethod} -> analysisMethod) (\s@SchemaSummary' {} a -> s {analysisMethod = a} :: SchemaSummary)

-- | The name for the schema object.
schemaSummary_name :: Lens.Lens' SchemaSummary Prelude.Text
schemaSummary_name = Lens.lens (\SchemaSummary' {name} -> name) (\s@SchemaSummary' {} a -> s {name = a} :: SchemaSummary)

-- | The type of schema object. The only valid schema type is currently
-- \`TABLE\`.
schemaSummary_type :: Lens.Lens' SchemaSummary SchemaType
schemaSummary_type = Lens.lens (\SchemaSummary' {type'} -> type') (\s@SchemaSummary' {} a -> s {type' = a} :: SchemaSummary)

-- | The unique account ID for the AWS account that owns the schema.
schemaSummary_creatorAccountId :: Lens.Lens' SchemaSummary Prelude.Text
schemaSummary_creatorAccountId = Lens.lens (\SchemaSummary' {creatorAccountId} -> creatorAccountId) (\s@SchemaSummary' {} a -> s {creatorAccountId = a} :: SchemaSummary)

-- | The time the schema object was created.
schemaSummary_createTime :: Lens.Lens' SchemaSummary Prelude.UTCTime
schemaSummary_createTime = Lens.lens (\SchemaSummary' {createTime} -> createTime) (\s@SchemaSummary' {} a -> s {createTime = a} :: SchemaSummary) Prelude.. Data._Time

-- | The time the schema object was last updated.
schemaSummary_updateTime :: Lens.Lens' SchemaSummary Prelude.UTCTime
schemaSummary_updateTime = Lens.lens (\SchemaSummary' {updateTime} -> updateTime) (\s@SchemaSummary' {} a -> s {updateTime = a} :: SchemaSummary) Prelude.. Data._Time

-- | The unique ID for the collaboration that the schema belongs to.
schemaSummary_collaborationId :: Lens.Lens' SchemaSummary Prelude.Text
schemaSummary_collaborationId = Lens.lens (\SchemaSummary' {collaborationId} -> collaborationId) (\s@SchemaSummary' {} a -> s {collaborationId = a} :: SchemaSummary)

-- | The unique ARN for the collaboration that the schema belongs to.
schemaSummary_collaborationArn :: Lens.Lens' SchemaSummary Prelude.Text
schemaSummary_collaborationArn = Lens.lens (\SchemaSummary' {collaborationArn} -> collaborationArn) (\s@SchemaSummary' {} a -> s {collaborationArn = a} :: SchemaSummary)

-- | The types of analysis rules that are associated with this schema object.
schemaSummary_analysisRuleTypes :: Lens.Lens' SchemaSummary [AnalysisRuleType]
schemaSummary_analysisRuleTypes = Lens.lens (\SchemaSummary' {analysisRuleTypes} -> analysisRuleTypes) (\s@SchemaSummary' {} a -> s {analysisRuleTypes = a} :: SchemaSummary) Prelude.. Lens.coerced

instance Data.FromJSON SchemaSummary where
  parseJSON =
    Data.withObject
      "SchemaSummary"
      ( \x ->
          SchemaSummary'
            Prelude.<$> (x Data..:? "analysisMethod")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "creatorAccountId")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "collaborationId")
            Prelude.<*> (x Data..: "collaborationArn")
            Prelude.<*> ( x
                            Data..:? "analysisRuleTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SchemaSummary where
  hashWithSalt _salt SchemaSummary' {..} =
    _salt
      `Prelude.hashWithSalt` analysisMethod
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` creatorAccountId
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` collaborationId
      `Prelude.hashWithSalt` collaborationArn
      `Prelude.hashWithSalt` analysisRuleTypes

instance Prelude.NFData SchemaSummary where
  rnf SchemaSummary' {..} =
    Prelude.rnf analysisMethod
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creatorAccountId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf collaborationId
      `Prelude.seq` Prelude.rnf collaborationArn
      `Prelude.seq` Prelude.rnf analysisRuleTypes

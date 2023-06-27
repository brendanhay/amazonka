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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTable where

import Amazonka.CleanRooms.Types.AnalysisMethod
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
import Amazonka.CleanRooms.Types.TableReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A table that has been configured for use in a collaboration.
--
-- /See:/ 'newConfiguredTable' smart constructor.
data ConfiguredTable = ConfiguredTable'
  { -- | A description for the configured table.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the configured table.
    id :: Prelude.Text,
    -- | The unique ARN for the configured table.
    arn :: Prelude.Text,
    -- | A name for the configured table.
    name :: Prelude.Text,
    -- | The AWS Glue table that this configured table represents.
    tableReference :: TableReference,
    -- | The time the configured table was created.
    createTime :: Data.POSIX,
    -- | The time the configured table was last updated
    updateTime :: Data.POSIX,
    -- | The types of analysis rules associated with this configured table. Valid
    -- values are \`AGGREGATION\` and \`LIST\`. Currently, only one analysis
    -- rule may be associated with a configured table.
    analysisRuleTypes :: [ConfiguredTableAnalysisRuleType],
    -- | The analysis method for the configured table. The only valid value is
    -- currently \`DIRECT_QUERY\`.
    analysisMethod :: AnalysisMethod,
    -- | The columns within the underlying AWS Glue table that can be utilized
    -- within collaborations.
    allowedColumns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'configuredTable_description' - A description for the configured table.
--
-- 'id', 'configuredTable_id' - The unique ID for the configured table.
--
-- 'arn', 'configuredTable_arn' - The unique ARN for the configured table.
--
-- 'name', 'configuredTable_name' - A name for the configured table.
--
-- 'tableReference', 'configuredTable_tableReference' - The AWS Glue table that this configured table represents.
--
-- 'createTime', 'configuredTable_createTime' - The time the configured table was created.
--
-- 'updateTime', 'configuredTable_updateTime' - The time the configured table was last updated
--
-- 'analysisRuleTypes', 'configuredTable_analysisRuleTypes' - The types of analysis rules associated with this configured table. Valid
-- values are \`AGGREGATION\` and \`LIST\`. Currently, only one analysis
-- rule may be associated with a configured table.
--
-- 'analysisMethod', 'configuredTable_analysisMethod' - The analysis method for the configured table. The only valid value is
-- currently \`DIRECT_QUERY\`.
--
-- 'allowedColumns', 'configuredTable_allowedColumns' - The columns within the underlying AWS Glue table that can be utilized
-- within collaborations.
newConfiguredTable ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'tableReference'
  TableReference ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'analysisMethod'
  AnalysisMethod ->
  -- | 'allowedColumns'
  Prelude.NonEmpty Prelude.Text ->
  ConfiguredTable
newConfiguredTable
  pId_
  pArn_
  pName_
  pTableReference_
  pCreateTime_
  pUpdateTime_
  pAnalysisMethod_
  pAllowedColumns_ =
    ConfiguredTable'
      { description = Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        name = pName_,
        tableReference = pTableReference_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        analysisRuleTypes = Prelude.mempty,
        analysisMethod = pAnalysisMethod_,
        allowedColumns =
          Lens.coerced Lens.# pAllowedColumns_
      }

-- | A description for the configured table.
configuredTable_description :: Lens.Lens' ConfiguredTable (Prelude.Maybe Prelude.Text)
configuredTable_description = Lens.lens (\ConfiguredTable' {description} -> description) (\s@ConfiguredTable' {} a -> s {description = a} :: ConfiguredTable)

-- | The unique ID for the configured table.
configuredTable_id :: Lens.Lens' ConfiguredTable Prelude.Text
configuredTable_id = Lens.lens (\ConfiguredTable' {id} -> id) (\s@ConfiguredTable' {} a -> s {id = a} :: ConfiguredTable)

-- | The unique ARN for the configured table.
configuredTable_arn :: Lens.Lens' ConfiguredTable Prelude.Text
configuredTable_arn = Lens.lens (\ConfiguredTable' {arn} -> arn) (\s@ConfiguredTable' {} a -> s {arn = a} :: ConfiguredTable)

-- | A name for the configured table.
configuredTable_name :: Lens.Lens' ConfiguredTable Prelude.Text
configuredTable_name = Lens.lens (\ConfiguredTable' {name} -> name) (\s@ConfiguredTable' {} a -> s {name = a} :: ConfiguredTable)

-- | The AWS Glue table that this configured table represents.
configuredTable_tableReference :: Lens.Lens' ConfiguredTable TableReference
configuredTable_tableReference = Lens.lens (\ConfiguredTable' {tableReference} -> tableReference) (\s@ConfiguredTable' {} a -> s {tableReference = a} :: ConfiguredTable)

-- | The time the configured table was created.
configuredTable_createTime :: Lens.Lens' ConfiguredTable Prelude.UTCTime
configuredTable_createTime = Lens.lens (\ConfiguredTable' {createTime} -> createTime) (\s@ConfiguredTable' {} a -> s {createTime = a} :: ConfiguredTable) Prelude.. Data._Time

-- | The time the configured table was last updated
configuredTable_updateTime :: Lens.Lens' ConfiguredTable Prelude.UTCTime
configuredTable_updateTime = Lens.lens (\ConfiguredTable' {updateTime} -> updateTime) (\s@ConfiguredTable' {} a -> s {updateTime = a} :: ConfiguredTable) Prelude.. Data._Time

-- | The types of analysis rules associated with this configured table. Valid
-- values are \`AGGREGATION\` and \`LIST\`. Currently, only one analysis
-- rule may be associated with a configured table.
configuredTable_analysisRuleTypes :: Lens.Lens' ConfiguredTable [ConfiguredTableAnalysisRuleType]
configuredTable_analysisRuleTypes = Lens.lens (\ConfiguredTable' {analysisRuleTypes} -> analysisRuleTypes) (\s@ConfiguredTable' {} a -> s {analysisRuleTypes = a} :: ConfiguredTable) Prelude.. Lens.coerced

-- | The analysis method for the configured table. The only valid value is
-- currently \`DIRECT_QUERY\`.
configuredTable_analysisMethod :: Lens.Lens' ConfiguredTable AnalysisMethod
configuredTable_analysisMethod = Lens.lens (\ConfiguredTable' {analysisMethod} -> analysisMethod) (\s@ConfiguredTable' {} a -> s {analysisMethod = a} :: ConfiguredTable)

-- | The columns within the underlying AWS Glue table that can be utilized
-- within collaborations.
configuredTable_allowedColumns :: Lens.Lens' ConfiguredTable (Prelude.NonEmpty Prelude.Text)
configuredTable_allowedColumns = Lens.lens (\ConfiguredTable' {allowedColumns} -> allowedColumns) (\s@ConfiguredTable' {} a -> s {allowedColumns = a} :: ConfiguredTable) Prelude.. Lens.coerced

instance Data.FromJSON ConfiguredTable where
  parseJSON =
    Data.withObject
      "ConfiguredTable"
      ( \x ->
          ConfiguredTable'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "tableReference")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> ( x
                            Data..:? "analysisRuleTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "analysisMethod")
            Prelude.<*> (x Data..: "allowedColumns")
      )

instance Prelude.Hashable ConfiguredTable where
  hashWithSalt _salt ConfiguredTable' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tableReference
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` analysisRuleTypes
      `Prelude.hashWithSalt` analysisMethod
      `Prelude.hashWithSalt` allowedColumns

instance Prelude.NFData ConfiguredTable where
  rnf ConfiguredTable' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tableReference
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf analysisRuleTypes
      `Prelude.seq` Prelude.rnf analysisMethod
      `Prelude.seq` Prelude.rnf allowedColumns

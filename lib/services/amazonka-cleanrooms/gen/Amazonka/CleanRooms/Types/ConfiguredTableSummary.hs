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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableSummary where

import Amazonka.CleanRooms.Types.AnalysisMethod
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configured table summary for the objects listed by the request.
--
-- /See:/ 'newConfiguredTableSummary' smart constructor.
data ConfiguredTableSummary = ConfiguredTableSummary'
  { -- | The unique ID of the configured table.
    id :: Prelude.Text,
    -- | The unique ARN of the configured table.
    arn :: Prelude.Text,
    -- | The name of the configured table.
    name :: Prelude.Text,
    -- | The time the configured table was created.
    createTime :: Data.POSIX,
    -- | The time the configured table was last updated.
    updateTime :: Data.POSIX,
    -- | The types of analysis rules associated with this configured table.
    analysisRuleTypes :: [ConfiguredTableAnalysisRuleType],
    -- | The analysis method for the configured tables. The only valid value is
    -- currently \`DIRECT_QUERY\`.
    analysisMethod :: AnalysisMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'configuredTableSummary_id' - The unique ID of the configured table.
--
-- 'arn', 'configuredTableSummary_arn' - The unique ARN of the configured table.
--
-- 'name', 'configuredTableSummary_name' - The name of the configured table.
--
-- 'createTime', 'configuredTableSummary_createTime' - The time the configured table was created.
--
-- 'updateTime', 'configuredTableSummary_updateTime' - The time the configured table was last updated.
--
-- 'analysisRuleTypes', 'configuredTableSummary_analysisRuleTypes' - The types of analysis rules associated with this configured table.
--
-- 'analysisMethod', 'configuredTableSummary_analysisMethod' - The analysis method for the configured tables. The only valid value is
-- currently \`DIRECT_QUERY\`.
newConfiguredTableSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'analysisMethod'
  AnalysisMethod ->
  ConfiguredTableSummary
newConfiguredTableSummary
  pId_
  pArn_
  pName_
  pCreateTime_
  pUpdateTime_
  pAnalysisMethod_ =
    ConfiguredTableSummary'
      { id = pId_,
        arn = pArn_,
        name = pName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        analysisRuleTypes = Prelude.mempty,
        analysisMethod = pAnalysisMethod_
      }

-- | The unique ID of the configured table.
configuredTableSummary_id :: Lens.Lens' ConfiguredTableSummary Prelude.Text
configuredTableSummary_id = Lens.lens (\ConfiguredTableSummary' {id} -> id) (\s@ConfiguredTableSummary' {} a -> s {id = a} :: ConfiguredTableSummary)

-- | The unique ARN of the configured table.
configuredTableSummary_arn :: Lens.Lens' ConfiguredTableSummary Prelude.Text
configuredTableSummary_arn = Lens.lens (\ConfiguredTableSummary' {arn} -> arn) (\s@ConfiguredTableSummary' {} a -> s {arn = a} :: ConfiguredTableSummary)

-- | The name of the configured table.
configuredTableSummary_name :: Lens.Lens' ConfiguredTableSummary Prelude.Text
configuredTableSummary_name = Lens.lens (\ConfiguredTableSummary' {name} -> name) (\s@ConfiguredTableSummary' {} a -> s {name = a} :: ConfiguredTableSummary)

-- | The time the configured table was created.
configuredTableSummary_createTime :: Lens.Lens' ConfiguredTableSummary Prelude.UTCTime
configuredTableSummary_createTime = Lens.lens (\ConfiguredTableSummary' {createTime} -> createTime) (\s@ConfiguredTableSummary' {} a -> s {createTime = a} :: ConfiguredTableSummary) Prelude.. Data._Time

-- | The time the configured table was last updated.
configuredTableSummary_updateTime :: Lens.Lens' ConfiguredTableSummary Prelude.UTCTime
configuredTableSummary_updateTime = Lens.lens (\ConfiguredTableSummary' {updateTime} -> updateTime) (\s@ConfiguredTableSummary' {} a -> s {updateTime = a} :: ConfiguredTableSummary) Prelude.. Data._Time

-- | The types of analysis rules associated with this configured table.
configuredTableSummary_analysisRuleTypes :: Lens.Lens' ConfiguredTableSummary [ConfiguredTableAnalysisRuleType]
configuredTableSummary_analysisRuleTypes = Lens.lens (\ConfiguredTableSummary' {analysisRuleTypes} -> analysisRuleTypes) (\s@ConfiguredTableSummary' {} a -> s {analysisRuleTypes = a} :: ConfiguredTableSummary) Prelude.. Lens.coerced

-- | The analysis method for the configured tables. The only valid value is
-- currently \`DIRECT_QUERY\`.
configuredTableSummary_analysisMethod :: Lens.Lens' ConfiguredTableSummary AnalysisMethod
configuredTableSummary_analysisMethod = Lens.lens (\ConfiguredTableSummary' {analysisMethod} -> analysisMethod) (\s@ConfiguredTableSummary' {} a -> s {analysisMethod = a} :: ConfiguredTableSummary)

instance Data.FromJSON ConfiguredTableSummary where
  parseJSON =
    Data.withObject
      "ConfiguredTableSummary"
      ( \x ->
          ConfiguredTableSummary'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> ( x
                            Data..:? "analysisRuleTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "analysisMethod")
      )

instance Prelude.Hashable ConfiguredTableSummary where
  hashWithSalt _salt ConfiguredTableSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` analysisRuleTypes
      `Prelude.hashWithSalt` analysisMethod

instance Prelude.NFData ConfiguredTableSummary where
  rnf ConfiguredTableSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf analysisRuleTypes
      `Prelude.seq` Prelude.rnf analysisMethod

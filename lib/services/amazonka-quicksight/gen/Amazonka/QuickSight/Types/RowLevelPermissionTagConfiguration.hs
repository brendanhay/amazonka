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
-- Module      : Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RowLevelPermissionTagRule
import Amazonka.QuickSight.Types.Status

-- | The configuration of tags on a dataset to set row-level security.
--
-- /See:/ 'newRowLevelPermissionTagConfiguration' smart constructor.
data RowLevelPermissionTagConfiguration = RowLevelPermissionTagConfiguration'
  { -- | The status of row-level security tags. If enabled, the status is
    -- @ENABLED@. If disabled, the status is @DISABLED@.
    status :: Prelude.Maybe Status,
    -- | A list of tag configuration rules to apply to a dataset. All tag
    -- configurations have the OR condition. Tags within each tile will be
    -- joined (AND). At least one rule in this structure must have all tag
    -- values assigned to it to apply Row-level security (RLS) to the dataset.
    tagRuleConfigurations :: Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Text)),
    -- | A set of rules associated with row-level security, such as the tag names
    -- and columns that they are assigned to.
    tagRules :: Prelude.NonEmpty RowLevelPermissionTagRule
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowLevelPermissionTagConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'rowLevelPermissionTagConfiguration_status' - The status of row-level security tags. If enabled, the status is
-- @ENABLED@. If disabled, the status is @DISABLED@.
--
-- 'tagRuleConfigurations', 'rowLevelPermissionTagConfiguration_tagRuleConfigurations' - A list of tag configuration rules to apply to a dataset. All tag
-- configurations have the OR condition. Tags within each tile will be
-- joined (AND). At least one rule in this structure must have all tag
-- values assigned to it to apply Row-level security (RLS) to the dataset.
--
-- 'tagRules', 'rowLevelPermissionTagConfiguration_tagRules' - A set of rules associated with row-level security, such as the tag names
-- and columns that they are assigned to.
newRowLevelPermissionTagConfiguration ::
  -- | 'tagRules'
  Prelude.NonEmpty RowLevelPermissionTagRule ->
  RowLevelPermissionTagConfiguration
newRowLevelPermissionTagConfiguration pTagRules_ =
  RowLevelPermissionTagConfiguration'
    { status =
        Prelude.Nothing,
      tagRuleConfigurations = Prelude.Nothing,
      tagRules =
        Lens.coerced Lens.# pTagRules_
    }

-- | The status of row-level security tags. If enabled, the status is
-- @ENABLED@. If disabled, the status is @DISABLED@.
rowLevelPermissionTagConfiguration_status :: Lens.Lens' RowLevelPermissionTagConfiguration (Prelude.Maybe Status)
rowLevelPermissionTagConfiguration_status = Lens.lens (\RowLevelPermissionTagConfiguration' {status} -> status) (\s@RowLevelPermissionTagConfiguration' {} a -> s {status = a} :: RowLevelPermissionTagConfiguration)

-- | A list of tag configuration rules to apply to a dataset. All tag
-- configurations have the OR condition. Tags within each tile will be
-- joined (AND). At least one rule in this structure must have all tag
-- values assigned to it to apply Row-level security (RLS) to the dataset.
rowLevelPermissionTagConfiguration_tagRuleConfigurations :: Lens.Lens' RowLevelPermissionTagConfiguration (Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Text)))
rowLevelPermissionTagConfiguration_tagRuleConfigurations = Lens.lens (\RowLevelPermissionTagConfiguration' {tagRuleConfigurations} -> tagRuleConfigurations) (\s@RowLevelPermissionTagConfiguration' {} a -> s {tagRuleConfigurations = a} :: RowLevelPermissionTagConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A set of rules associated with row-level security, such as the tag names
-- and columns that they are assigned to.
rowLevelPermissionTagConfiguration_tagRules :: Lens.Lens' RowLevelPermissionTagConfiguration (Prelude.NonEmpty RowLevelPermissionTagRule)
rowLevelPermissionTagConfiguration_tagRules = Lens.lens (\RowLevelPermissionTagConfiguration' {tagRules} -> tagRules) (\s@RowLevelPermissionTagConfiguration' {} a -> s {tagRules = a} :: RowLevelPermissionTagConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    RowLevelPermissionTagConfiguration
  where
  parseJSON =
    Data.withObject
      "RowLevelPermissionTagConfiguration"
      ( \x ->
          RowLevelPermissionTagConfiguration'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TagRuleConfigurations")
            Prelude.<*> (x Data..: "TagRules")
      )

instance
  Prelude.Hashable
    RowLevelPermissionTagConfiguration
  where
  hashWithSalt
    _salt
    RowLevelPermissionTagConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tagRuleConfigurations
        `Prelude.hashWithSalt` tagRules

instance
  Prelude.NFData
    RowLevelPermissionTagConfiguration
  where
  rnf RowLevelPermissionTagConfiguration' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf tagRuleConfigurations
      `Prelude.seq` Prelude.rnf tagRules

instance
  Data.ToJSON
    RowLevelPermissionTagConfiguration
  where
  toJSON RowLevelPermissionTagConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            ("TagRuleConfigurations" Data..=)
              Prelude.<$> tagRuleConfigurations,
            Prelude.Just ("TagRules" Data..= tagRules)
          ]
      )

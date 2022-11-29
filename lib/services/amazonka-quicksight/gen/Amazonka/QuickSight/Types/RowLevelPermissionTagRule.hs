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
-- Module      : Amazonka.QuickSight.Types.RowLevelPermissionTagRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowLevelPermissionTagRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of rules associated with a tag.
--
-- /See:/ 'newRowLevelPermissionTagRule' smart constructor.
data RowLevelPermissionTagRule = RowLevelPermissionTagRule'
  { -- | A string that you want to use to filter by all the values in a column in
    -- the dataset and don’t want to list the values one by one. For example,
    -- you can use an asterisk as your match all value.
    matchAllValue :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A string that you want to use to delimit the values when you pass the
    -- values at run time. For example, you can delimit the values with a
    -- comma.
    tagMultiValueDelimiter :: Prelude.Maybe Prelude.Text,
    -- | The unique key for a tag.
    tagKey :: Prelude.Text,
    -- | The column name that a tag key is assigned to.
    columnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowLevelPermissionTagRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchAllValue', 'rowLevelPermissionTagRule_matchAllValue' - A string that you want to use to filter by all the values in a column in
-- the dataset and don’t want to list the values one by one. For example,
-- you can use an asterisk as your match all value.
--
-- 'tagMultiValueDelimiter', 'rowLevelPermissionTagRule_tagMultiValueDelimiter' - A string that you want to use to delimit the values when you pass the
-- values at run time. For example, you can delimit the values with a
-- comma.
--
-- 'tagKey', 'rowLevelPermissionTagRule_tagKey' - The unique key for a tag.
--
-- 'columnName', 'rowLevelPermissionTagRule_columnName' - The column name that a tag key is assigned to.
newRowLevelPermissionTagRule ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'columnName'
  Prelude.Text ->
  RowLevelPermissionTagRule
newRowLevelPermissionTagRule pTagKey_ pColumnName_ =
  RowLevelPermissionTagRule'
    { matchAllValue =
        Prelude.Nothing,
      tagMultiValueDelimiter = Prelude.Nothing,
      tagKey = pTagKey_,
      columnName = pColumnName_
    }

-- | A string that you want to use to filter by all the values in a column in
-- the dataset and don’t want to list the values one by one. For example,
-- you can use an asterisk as your match all value.
rowLevelPermissionTagRule_matchAllValue :: Lens.Lens' RowLevelPermissionTagRule (Prelude.Maybe Prelude.Text)
rowLevelPermissionTagRule_matchAllValue = Lens.lens (\RowLevelPermissionTagRule' {matchAllValue} -> matchAllValue) (\s@RowLevelPermissionTagRule' {} a -> s {matchAllValue = a} :: RowLevelPermissionTagRule) Prelude.. Lens.mapping Core._Sensitive

-- | A string that you want to use to delimit the values when you pass the
-- values at run time. For example, you can delimit the values with a
-- comma.
rowLevelPermissionTagRule_tagMultiValueDelimiter :: Lens.Lens' RowLevelPermissionTagRule (Prelude.Maybe Prelude.Text)
rowLevelPermissionTagRule_tagMultiValueDelimiter = Lens.lens (\RowLevelPermissionTagRule' {tagMultiValueDelimiter} -> tagMultiValueDelimiter) (\s@RowLevelPermissionTagRule' {} a -> s {tagMultiValueDelimiter = a} :: RowLevelPermissionTagRule)

-- | The unique key for a tag.
rowLevelPermissionTagRule_tagKey :: Lens.Lens' RowLevelPermissionTagRule Prelude.Text
rowLevelPermissionTagRule_tagKey = Lens.lens (\RowLevelPermissionTagRule' {tagKey} -> tagKey) (\s@RowLevelPermissionTagRule' {} a -> s {tagKey = a} :: RowLevelPermissionTagRule)

-- | The column name that a tag key is assigned to.
rowLevelPermissionTagRule_columnName :: Lens.Lens' RowLevelPermissionTagRule Prelude.Text
rowLevelPermissionTagRule_columnName = Lens.lens (\RowLevelPermissionTagRule' {columnName} -> columnName) (\s@RowLevelPermissionTagRule' {} a -> s {columnName = a} :: RowLevelPermissionTagRule)

instance Core.FromJSON RowLevelPermissionTagRule where
  parseJSON =
    Core.withObject
      "RowLevelPermissionTagRule"
      ( \x ->
          RowLevelPermissionTagRule'
            Prelude.<$> (x Core..:? "MatchAllValue")
            Prelude.<*> (x Core..:? "TagMultiValueDelimiter")
            Prelude.<*> (x Core..: "TagKey")
            Prelude.<*> (x Core..: "ColumnName")
      )

instance Prelude.Hashable RowLevelPermissionTagRule where
  hashWithSalt _salt RowLevelPermissionTagRule' {..} =
    _salt `Prelude.hashWithSalt` matchAllValue
      `Prelude.hashWithSalt` tagMultiValueDelimiter
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` columnName

instance Prelude.NFData RowLevelPermissionTagRule where
  rnf RowLevelPermissionTagRule' {..} =
    Prelude.rnf matchAllValue
      `Prelude.seq` Prelude.rnf tagMultiValueDelimiter
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf columnName

instance Core.ToJSON RowLevelPermissionTagRule where
  toJSON RowLevelPermissionTagRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MatchAllValue" Core..=) Prelude.<$> matchAllValue,
            ("TagMultiValueDelimiter" Core..=)
              Prelude.<$> tagMultiValueDelimiter,
            Prelude.Just ("TagKey" Core..= tagKey),
            Prelude.Just ("ColumnName" Core..= columnName)
          ]
      )

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
-- Maintainer  : Brendan Hay
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
      tagRules =
        Lens.coerced Lens.# pTagRules_
    }

-- | The status of row-level security tags. If enabled, the status is
-- @ENABLED@. If disabled, the status is @DISABLED@.
rowLevelPermissionTagConfiguration_status :: Lens.Lens' RowLevelPermissionTagConfiguration (Prelude.Maybe Status)
rowLevelPermissionTagConfiguration_status = Lens.lens (\RowLevelPermissionTagConfiguration' {status} -> status) (\s@RowLevelPermissionTagConfiguration' {} a -> s {status = a} :: RowLevelPermissionTagConfiguration)

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
        `Prelude.hashWithSalt` tagRules

instance
  Prelude.NFData
    RowLevelPermissionTagConfiguration
  where
  rnf RowLevelPermissionTagConfiguration' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf tagRules

instance
  Data.ToJSON
    RowLevelPermissionTagConfiguration
  where
  toJSON RowLevelPermissionTagConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("TagRules" Data..= tagRules)
          ]
      )

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
-- Module      : Amazonka.QuickSight.Types.RowLevelPermissionDataSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowLevelPermissionDataSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RowLevelPermissionFormatVersion
import Amazonka.QuickSight.Types.RowLevelPermissionPolicy
import Amazonka.QuickSight.Types.Status

-- | Information about a dataset that contains permissions for row-level
-- security (RLS). The permissions dataset maps fields to users or groups.
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/restrict-access-to-a-data-set-using-row-level-security.html Using Row-Level Security (RLS) to Restrict Access to a Dataset>
-- in the /Amazon QuickSight User Guide/.
--
-- The option to deny permissions by setting @PermissionPolicy@ to
-- @DENY_ACCESS@ is not supported for new RLS datasets.
--
-- /See:/ 'newRowLevelPermissionDataSet' smart constructor.
data RowLevelPermissionDataSet = RowLevelPermissionDataSet'
  { -- | The user or group rules associated with the dataset that contains
    -- permissions for RLS.
    --
    -- By default, @FormatVersion@ is @VERSION_1@. When @FormatVersion@ is
    -- @VERSION_1@, @UserName@ and @GroupName@ are required. When
    -- @FormatVersion@ is @VERSION_2@, @UserARN@ and @GroupARN@ are required,
    -- and @Namespace@ must not exist.
    formatVersion :: Prelude.Maybe RowLevelPermissionFormatVersion,
    -- | The status of the row-level security permission dataset. If enabled, the
    -- status is @ENABLED@. If disabled, the status is @DISABLED@.
    status :: Prelude.Maybe Status,
    -- | The namespace associated with the dataset that contains permissions for
    -- RLS.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset that contains permissions
    -- for RLS.
    arn :: Prelude.Text,
    -- | The type of permissions to use when interpreting the permissions for
    -- RLS. @DENY_ACCESS@ is included for backward compatibility only.
    permissionPolicy :: RowLevelPermissionPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowLevelPermissionDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatVersion', 'rowLevelPermissionDataSet_formatVersion' - The user or group rules associated with the dataset that contains
-- permissions for RLS.
--
-- By default, @FormatVersion@ is @VERSION_1@. When @FormatVersion@ is
-- @VERSION_1@, @UserName@ and @GroupName@ are required. When
-- @FormatVersion@ is @VERSION_2@, @UserARN@ and @GroupARN@ are required,
-- and @Namespace@ must not exist.
--
-- 'status', 'rowLevelPermissionDataSet_status' - The status of the row-level security permission dataset. If enabled, the
-- status is @ENABLED@. If disabled, the status is @DISABLED@.
--
-- 'namespace', 'rowLevelPermissionDataSet_namespace' - The namespace associated with the dataset that contains permissions for
-- RLS.
--
-- 'arn', 'rowLevelPermissionDataSet_arn' - The Amazon Resource Name (ARN) of the dataset that contains permissions
-- for RLS.
--
-- 'permissionPolicy', 'rowLevelPermissionDataSet_permissionPolicy' - The type of permissions to use when interpreting the permissions for
-- RLS. @DENY_ACCESS@ is included for backward compatibility only.
newRowLevelPermissionDataSet ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'permissionPolicy'
  RowLevelPermissionPolicy ->
  RowLevelPermissionDataSet
newRowLevelPermissionDataSet pArn_ pPermissionPolicy_ =
  RowLevelPermissionDataSet'
    { formatVersion =
        Prelude.Nothing,
      status = Prelude.Nothing,
      namespace = Prelude.Nothing,
      arn = pArn_,
      permissionPolicy = pPermissionPolicy_
    }

-- | The user or group rules associated with the dataset that contains
-- permissions for RLS.
--
-- By default, @FormatVersion@ is @VERSION_1@. When @FormatVersion@ is
-- @VERSION_1@, @UserName@ and @GroupName@ are required. When
-- @FormatVersion@ is @VERSION_2@, @UserARN@ and @GroupARN@ are required,
-- and @Namespace@ must not exist.
rowLevelPermissionDataSet_formatVersion :: Lens.Lens' RowLevelPermissionDataSet (Prelude.Maybe RowLevelPermissionFormatVersion)
rowLevelPermissionDataSet_formatVersion = Lens.lens (\RowLevelPermissionDataSet' {formatVersion} -> formatVersion) (\s@RowLevelPermissionDataSet' {} a -> s {formatVersion = a} :: RowLevelPermissionDataSet)

-- | The status of the row-level security permission dataset. If enabled, the
-- status is @ENABLED@. If disabled, the status is @DISABLED@.
rowLevelPermissionDataSet_status :: Lens.Lens' RowLevelPermissionDataSet (Prelude.Maybe Status)
rowLevelPermissionDataSet_status = Lens.lens (\RowLevelPermissionDataSet' {status} -> status) (\s@RowLevelPermissionDataSet' {} a -> s {status = a} :: RowLevelPermissionDataSet)

-- | The namespace associated with the dataset that contains permissions for
-- RLS.
rowLevelPermissionDataSet_namespace :: Lens.Lens' RowLevelPermissionDataSet (Prelude.Maybe Prelude.Text)
rowLevelPermissionDataSet_namespace = Lens.lens (\RowLevelPermissionDataSet' {namespace} -> namespace) (\s@RowLevelPermissionDataSet' {} a -> s {namespace = a} :: RowLevelPermissionDataSet)

-- | The Amazon Resource Name (ARN) of the dataset that contains permissions
-- for RLS.
rowLevelPermissionDataSet_arn :: Lens.Lens' RowLevelPermissionDataSet Prelude.Text
rowLevelPermissionDataSet_arn = Lens.lens (\RowLevelPermissionDataSet' {arn} -> arn) (\s@RowLevelPermissionDataSet' {} a -> s {arn = a} :: RowLevelPermissionDataSet)

-- | The type of permissions to use when interpreting the permissions for
-- RLS. @DENY_ACCESS@ is included for backward compatibility only.
rowLevelPermissionDataSet_permissionPolicy :: Lens.Lens' RowLevelPermissionDataSet RowLevelPermissionPolicy
rowLevelPermissionDataSet_permissionPolicy = Lens.lens (\RowLevelPermissionDataSet' {permissionPolicy} -> permissionPolicy) (\s@RowLevelPermissionDataSet' {} a -> s {permissionPolicy = a} :: RowLevelPermissionDataSet)

instance Data.FromJSON RowLevelPermissionDataSet where
  parseJSON =
    Data.withObject
      "RowLevelPermissionDataSet"
      ( \x ->
          RowLevelPermissionDataSet'
            Prelude.<$> (x Data..:? "FormatVersion")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Namespace")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "PermissionPolicy")
      )

instance Prelude.Hashable RowLevelPermissionDataSet where
  hashWithSalt _salt RowLevelPermissionDataSet' {..} =
    _salt `Prelude.hashWithSalt` formatVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` permissionPolicy

instance Prelude.NFData RowLevelPermissionDataSet where
  rnf RowLevelPermissionDataSet' {..} =
    Prelude.rnf formatVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf permissionPolicy

instance Data.ToJSON RowLevelPermissionDataSet where
  toJSON RowLevelPermissionDataSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatVersion" Data..=) Prelude.<$> formatVersion,
            ("Status" Data..=) Prelude.<$> status,
            ("Namespace" Data..=) Prelude.<$> namespace,
            Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("PermissionPolicy" Data..= permissionPolicy)
          ]
      )

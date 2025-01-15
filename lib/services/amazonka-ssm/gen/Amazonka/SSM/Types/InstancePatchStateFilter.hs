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
-- Module      : Amazonka.SSM.Types.InstancePatchStateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstancePatchStateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InstancePatchStateOperatorType

-- | Defines a filter used in DescribeInstancePatchStatesForPatchGroup to
-- scope down the information returned by the API.
--
-- __Example__: To filter for all managed nodes in a patch group having
-- more than three patches with a @FailedCount@ status, use the following
-- for the filter:
--
-- -   Value for @Key@: @FailedCount@
--
-- -   Value for @Type@: @GreaterThan@
--
-- -   Value for @Values@: @3@
--
-- /See:/ 'newInstancePatchStateFilter' smart constructor.
data InstancePatchStateFilter = InstancePatchStateFilter'
  { -- | The key for the filter. Supported values include the following:
    --
    -- -   @InstalledCount@
    --
    -- -   @InstalledOtherCount@
    --
    -- -   @InstalledPendingRebootCount@
    --
    -- -   @InstalledRejectedCount@
    --
    -- -   @MissingCount@
    --
    -- -   @FailedCount@
    --
    -- -   @UnreportedNotApplicableCount@
    --
    -- -   @NotApplicableCount@
    key :: Prelude.Text,
    -- | The value for the filter. Must be an integer greater than or equal to 0.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The type of comparison that should be performed for the value.
    type' :: InstancePatchStateOperatorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstancePatchStateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instancePatchStateFilter_key' - The key for the filter. Supported values include the following:
--
-- -   @InstalledCount@
--
-- -   @InstalledOtherCount@
--
-- -   @InstalledPendingRebootCount@
--
-- -   @InstalledRejectedCount@
--
-- -   @MissingCount@
--
-- -   @FailedCount@
--
-- -   @UnreportedNotApplicableCount@
--
-- -   @NotApplicableCount@
--
-- 'values', 'instancePatchStateFilter_values' - The value for the filter. Must be an integer greater than or equal to 0.
--
-- 'type'', 'instancePatchStateFilter_type' - The type of comparison that should be performed for the value.
newInstancePatchStateFilter ::
  -- | 'key'
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'type''
  InstancePatchStateOperatorType ->
  InstancePatchStateFilter
newInstancePatchStateFilter pKey_ pValues_ pType_ =
  InstancePatchStateFilter'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_,
      type' = pType_
    }

-- | The key for the filter. Supported values include the following:
--
-- -   @InstalledCount@
--
-- -   @InstalledOtherCount@
--
-- -   @InstalledPendingRebootCount@
--
-- -   @InstalledRejectedCount@
--
-- -   @MissingCount@
--
-- -   @FailedCount@
--
-- -   @UnreportedNotApplicableCount@
--
-- -   @NotApplicableCount@
instancePatchStateFilter_key :: Lens.Lens' InstancePatchStateFilter Prelude.Text
instancePatchStateFilter_key = Lens.lens (\InstancePatchStateFilter' {key} -> key) (\s@InstancePatchStateFilter' {} a -> s {key = a} :: InstancePatchStateFilter)

-- | The value for the filter. Must be an integer greater than or equal to 0.
instancePatchStateFilter_values :: Lens.Lens' InstancePatchStateFilter (Prelude.NonEmpty Prelude.Text)
instancePatchStateFilter_values = Lens.lens (\InstancePatchStateFilter' {values} -> values) (\s@InstancePatchStateFilter' {} a -> s {values = a} :: InstancePatchStateFilter) Prelude.. Lens.coerced

-- | The type of comparison that should be performed for the value.
instancePatchStateFilter_type :: Lens.Lens' InstancePatchStateFilter InstancePatchStateOperatorType
instancePatchStateFilter_type = Lens.lens (\InstancePatchStateFilter' {type'} -> type') (\s@InstancePatchStateFilter' {} a -> s {type' = a} :: InstancePatchStateFilter)

instance Prelude.Hashable InstancePatchStateFilter where
  hashWithSalt _salt InstancePatchStateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` type'

instance Prelude.NFData InstancePatchStateFilter where
  rnf InstancePatchStateFilter' {..} =
    Prelude.rnf key `Prelude.seq`
      Prelude.rnf values `Prelude.seq`
        Prelude.rnf type'

instance Data.ToJSON InstancePatchStateFilter where
  toJSON InstancePatchStateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values),
            Prelude.Just ("Type" Data..= type')
          ]
      )

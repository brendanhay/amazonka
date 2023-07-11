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
-- Module      : Amazonka.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParameterStringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- /See:/ 'newParameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { -- | For all filters used with DescribeParameters, valid options include
    -- @Equals@ and @BeginsWith@. The @Name@ filter additionally supports the
    -- @Contains@ option. (Exception: For filters using the key @Path@, valid
    -- options include @Recursive@ and @OneLevel@.)
    --
    -- For filters used with GetParametersByPath, valid options include
    -- @Equals@ and @BeginsWith@. (Exception: For filters using @Label@ as the
    -- Key name, the only valid option is @Equals@.)
    option :: Prelude.Maybe Prelude.Text,
    -- | The value you want to search for.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the filter.
    --
    -- The @ParameterStringFilter@ object is used by the DescribeParameters and
    -- GetParametersByPath API operations. However, not all of the pattern
    -- values listed for @Key@ can be used with both operations.
    --
    -- For @DescribeParameters@, all of the listed patterns are valid except
    -- @Label@.
    --
    -- For @GetParametersByPath@, the following patterns listed for @Key@
    -- aren\'t valid: @tag@, @DataType@, @Name@, @Path@, and @Tier@.
    --
    -- For examples of Amazon Web Services CLI commands demonstrating valid
    -- parameter filter constructions, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'option', 'parameterStringFilter_option' - For all filters used with DescribeParameters, valid options include
-- @Equals@ and @BeginsWith@. The @Name@ filter additionally supports the
-- @Contains@ option. (Exception: For filters using the key @Path@, valid
-- options include @Recursive@ and @OneLevel@.)
--
-- For filters used with GetParametersByPath, valid options include
-- @Equals@ and @BeginsWith@. (Exception: For filters using @Label@ as the
-- Key name, the only valid option is @Equals@.)
--
-- 'values', 'parameterStringFilter_values' - The value you want to search for.
--
-- 'key', 'parameterStringFilter_key' - The name of the filter.
--
-- The @ParameterStringFilter@ object is used by the DescribeParameters and
-- GetParametersByPath API operations. However, not all of the pattern
-- values listed for @Key@ can be used with both operations.
--
-- For @DescribeParameters@, all of the listed patterns are valid except
-- @Label@.
--
-- For @GetParametersByPath@, the following patterns listed for @Key@
-- aren\'t valid: @tag@, @DataType@, @Name@, @Path@, and @Tier@.
--
-- For examples of Amazon Web Services CLI commands demonstrating valid
-- parameter filter constructions, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
newParameterStringFilter ::
  -- | 'key'
  Prelude.Text ->
  ParameterStringFilter
newParameterStringFilter pKey_ =
  ParameterStringFilter'
    { option = Prelude.Nothing,
      values = Prelude.Nothing,
      key = pKey_
    }

-- | For all filters used with DescribeParameters, valid options include
-- @Equals@ and @BeginsWith@. The @Name@ filter additionally supports the
-- @Contains@ option. (Exception: For filters using the key @Path@, valid
-- options include @Recursive@ and @OneLevel@.)
--
-- For filters used with GetParametersByPath, valid options include
-- @Equals@ and @BeginsWith@. (Exception: For filters using @Label@ as the
-- Key name, the only valid option is @Equals@.)
parameterStringFilter_option :: Lens.Lens' ParameterStringFilter (Prelude.Maybe Prelude.Text)
parameterStringFilter_option = Lens.lens (\ParameterStringFilter' {option} -> option) (\s@ParameterStringFilter' {} a -> s {option = a} :: ParameterStringFilter)

-- | The value you want to search for.
parameterStringFilter_values :: Lens.Lens' ParameterStringFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
parameterStringFilter_values = Lens.lens (\ParameterStringFilter' {values} -> values) (\s@ParameterStringFilter' {} a -> s {values = a} :: ParameterStringFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter.
--
-- The @ParameterStringFilter@ object is used by the DescribeParameters and
-- GetParametersByPath API operations. However, not all of the pattern
-- values listed for @Key@ can be used with both operations.
--
-- For @DescribeParameters@, all of the listed patterns are valid except
-- @Label@.
--
-- For @GetParametersByPath@, the following patterns listed for @Key@
-- aren\'t valid: @tag@, @DataType@, @Name@, @Path@, and @Tier@.
--
-- For examples of Amazon Web Services CLI commands demonstrating valid
-- parameter filter constructions, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
parameterStringFilter_key :: Lens.Lens' ParameterStringFilter Prelude.Text
parameterStringFilter_key = Lens.lens (\ParameterStringFilter' {key} -> key) (\s@ParameterStringFilter' {} a -> s {key = a} :: ParameterStringFilter)

instance Prelude.Hashable ParameterStringFilter where
  hashWithSalt _salt ParameterStringFilter' {..} =
    _salt
      `Prelude.hashWithSalt` option
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` key

instance Prelude.NFData ParameterStringFilter where
  rnf ParameterStringFilter' {..} =
    Prelude.rnf option
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ParameterStringFilter where
  toJSON ParameterStringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Option" Data..=) Prelude.<$> option,
            ("Values" Data..=) Prelude.<$> values,
            Prelude.Just ("Key" Data..= key)
          ]
      )

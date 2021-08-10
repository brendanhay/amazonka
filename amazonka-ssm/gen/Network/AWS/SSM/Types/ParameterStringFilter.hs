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
-- Module      : Network.AWS.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterStringFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- /See:/ 'newParameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { -- | The value you want to search for.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | For all filters used with DescribeParameters, valid options include
    -- @Equals@ and @BeginsWith@. The @Name@ filter additionally supports the
    -- @Contains@ option. (Exception: For filters using the key @Path@, valid
    -- options include @Recursive@ and @OneLevel@.)
    --
    -- For filters used with GetParametersByPath, valid options include
    -- @Equals@ and @BeginsWith@. (Exception: For filters using @Label@ as the
    -- Key name, the only valid option is @Equals@.)
    option :: Prelude.Maybe Prelude.Text,
    -- | The name of the filter.
    --
    -- The @ParameterStringFilter@ object is used by the DescribeParameters and
    -- GetParametersByPath API actions. However, not all of the pattern values
    -- listed for @Key@ can be used with both actions.
    --
    -- For @DescribeActions@, all of the listed patterns are valid, with the
    -- exception of @Label@.
    --
    -- For @GetParametersByPath@, the following patterns listed for @Key@ are
    -- not valid: @tag@, @Name@, @Path@, and @Tier@.
    --
    -- For examples of CLI commands demonstrating valid parameter filter
    -- constructions, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
    -- in the /AWS Systems Manager User Guide/.
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
-- 'values', 'parameterStringFilter_values' - The value you want to search for.
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
-- 'key', 'parameterStringFilter_key' - The name of the filter.
--
-- The @ParameterStringFilter@ object is used by the DescribeParameters and
-- GetParametersByPath API actions. However, not all of the pattern values
-- listed for @Key@ can be used with both actions.
--
-- For @DescribeActions@, all of the listed patterns are valid, with the
-- exception of @Label@.
--
-- For @GetParametersByPath@, the following patterns listed for @Key@ are
-- not valid: @tag@, @Name@, @Path@, and @Tier@.
--
-- For examples of CLI commands demonstrating valid parameter filter
-- constructions, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
-- in the /AWS Systems Manager User Guide/.
newParameterStringFilter ::
  -- | 'key'
  Prelude.Text ->
  ParameterStringFilter
newParameterStringFilter pKey_ =
  ParameterStringFilter'
    { values = Prelude.Nothing,
      option = Prelude.Nothing,
      key = pKey_
    }

-- | The value you want to search for.
parameterStringFilter_values :: Lens.Lens' ParameterStringFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
parameterStringFilter_values = Lens.lens (\ParameterStringFilter' {values} -> values) (\s@ParameterStringFilter' {} a -> s {values = a} :: ParameterStringFilter) Prelude.. Lens.mapping Lens._Coerce

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

-- | The name of the filter.
--
-- The @ParameterStringFilter@ object is used by the DescribeParameters and
-- GetParametersByPath API actions. However, not all of the pattern values
-- listed for @Key@ can be used with both actions.
--
-- For @DescribeActions@, all of the listed patterns are valid, with the
-- exception of @Label@.
--
-- For @GetParametersByPath@, the following patterns listed for @Key@ are
-- not valid: @tag@, @Name@, @Path@, and @Tier@.
--
-- For examples of CLI commands demonstrating valid parameter filter
-- constructions, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters>
-- in the /AWS Systems Manager User Guide/.
parameterStringFilter_key :: Lens.Lens' ParameterStringFilter Prelude.Text
parameterStringFilter_key = Lens.lens (\ParameterStringFilter' {key} -> key) (\s@ParameterStringFilter' {} a -> s {key = a} :: ParameterStringFilter)

instance Prelude.Hashable ParameterStringFilter

instance Prelude.NFData ParameterStringFilter

instance Core.ToJSON ParameterStringFilter where
  toJSON ParameterStringFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Option" Core..=) Prelude.<$> option,
            Prelude.Just ("Key" Core..= key)
          ]
      )

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
-- Module      : Amazonka.ResourceGroups.Types.GroupConfigurationParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupConfigurationParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A parameter for a group configuration item. For details about group
-- service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- /See:/ 'newGroupConfigurationParameter' smart constructor.
data GroupConfigurationParameter = GroupConfigurationParameter'
  { -- | The value or values to be used for the specified parameter. For the list
    -- of values you can use with each parameter, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the group configuration parameter. For the list of
    -- parameters that you can use with each configuration item type, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupConfigurationParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'groupConfigurationParameter_values' - The value or values to be used for the specified parameter. For the list
-- of values you can use with each parameter, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
--
-- 'name', 'groupConfigurationParameter_name' - The name of the group configuration parameter. For the list of
-- parameters that you can use with each configuration item type, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
newGroupConfigurationParameter ::
  -- | 'name'
  Prelude.Text ->
  GroupConfigurationParameter
newGroupConfigurationParameter pName_ =
  GroupConfigurationParameter'
    { values =
        Prelude.Nothing,
      name = pName_
    }

-- | The value or values to be used for the specified parameter. For the list
-- of values you can use with each parameter, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationParameter_values :: Lens.Lens' GroupConfigurationParameter (Prelude.Maybe [Prelude.Text])
groupConfigurationParameter_values = Lens.lens (\GroupConfigurationParameter' {values} -> values) (\s@GroupConfigurationParameter' {} a -> s {values = a} :: GroupConfigurationParameter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the group configuration parameter. For the list of
-- parameters that you can use with each configuration item type, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationParameter_name :: Lens.Lens' GroupConfigurationParameter Prelude.Text
groupConfigurationParameter_name = Lens.lens (\GroupConfigurationParameter' {name} -> name) (\s@GroupConfigurationParameter' {} a -> s {name = a} :: GroupConfigurationParameter)

instance Data.FromJSON GroupConfigurationParameter where
  parseJSON =
    Data.withObject
      "GroupConfigurationParameter"
      ( \x ->
          GroupConfigurationParameter'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable GroupConfigurationParameter where
  hashWithSalt _salt GroupConfigurationParameter' {..} =
    _salt `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` name

instance Prelude.NFData GroupConfigurationParameter where
  rnf GroupConfigurationParameter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Data.ToJSON GroupConfigurationParameter where
  toJSON GroupConfigurationParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Values" Data..=) Prelude.<$> values,
            Prelude.Just ("Name" Data..= name)
          ]
      )

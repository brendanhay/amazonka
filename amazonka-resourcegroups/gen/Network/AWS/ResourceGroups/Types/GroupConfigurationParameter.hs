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
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A parameter for a group configuration item. For details about group
-- service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- /See:/ 'newGroupConfigurationParameter' smart constructor.
data GroupConfigurationParameter = GroupConfigurationParameter'
  { -- | The value or values to be used for the specified parameter. For the list
    -- of values you can use with each parameter, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    values :: Core.Maybe [Core.Text],
    -- | The name of the group configuration parameter. For the list of
    -- parameters that you can use with each configuration item type, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GroupConfigurationParameter
newGroupConfigurationParameter pName_ =
  GroupConfigurationParameter'
    { values = Core.Nothing,
      name = pName_
    }

-- | The value or values to be used for the specified parameter. For the list
-- of values you can use with each parameter, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationParameter_values :: Lens.Lens' GroupConfigurationParameter (Core.Maybe [Core.Text])
groupConfigurationParameter_values = Lens.lens (\GroupConfigurationParameter' {values} -> values) (\s@GroupConfigurationParameter' {} a -> s {values = a} :: GroupConfigurationParameter) Core.. Lens.mapping Lens._Coerce

-- | The name of the group configuration parameter. For the list of
-- parameters that you can use with each configuration item type, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationParameter_name :: Lens.Lens' GroupConfigurationParameter Core.Text
groupConfigurationParameter_name = Lens.lens (\GroupConfigurationParameter' {name} -> name) (\s@GroupConfigurationParameter' {} a -> s {name = a} :: GroupConfigurationParameter)

instance Core.FromJSON GroupConfigurationParameter where
  parseJSON =
    Core.withObject
      "GroupConfigurationParameter"
      ( \x ->
          GroupConfigurationParameter'
            Core.<$> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable GroupConfigurationParameter

instance Core.NFData GroupConfigurationParameter

instance Core.ToJSON GroupConfigurationParameter where
  toJSON GroupConfigurationParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Values" Core..=) Core.<$> values,
            Core.Just ("Name" Core..= name)
          ]
      )

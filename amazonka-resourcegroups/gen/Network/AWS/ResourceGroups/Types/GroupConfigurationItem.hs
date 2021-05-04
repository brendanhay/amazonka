{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameter

-- | An item in a group configuration. A group service configuration can have
-- one or more items. For details about group service configuration syntax,
-- see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- /See:/ 'newGroupConfigurationItem' smart constructor.
data GroupConfigurationItem = GroupConfigurationItem'
  { -- | A collection of parameters for this group configuration item. For the
    -- list of parameters that you can use with each configuration item type,
    -- see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    parameters :: Prelude.Maybe [GroupConfigurationParameter],
    -- | Specifies the type of group configuration item. Each item must have a
    -- unique value for @type@. For the list of types that you can specify for
    -- a configuration item, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GroupConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'groupConfigurationItem_parameters' - A collection of parameters for this group configuration item. For the
-- list of parameters that you can use with each configuration item type,
-- see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
--
-- 'type'', 'groupConfigurationItem_type' - Specifies the type of group configuration item. Each item must have a
-- unique value for @type@. For the list of types that you can specify for
-- a configuration item, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
newGroupConfigurationItem ::
  -- | 'type''
  Prelude.Text ->
  GroupConfigurationItem
newGroupConfigurationItem pType_ =
  GroupConfigurationItem'
    { parameters =
        Prelude.Nothing,
      type' = pType_
    }

-- | A collection of parameters for this group configuration item. For the
-- list of parameters that you can use with each configuration item type,
-- see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationItem_parameters :: Lens.Lens' GroupConfigurationItem (Prelude.Maybe [GroupConfigurationParameter])
groupConfigurationItem_parameters = Lens.lens (\GroupConfigurationItem' {parameters} -> parameters) (\s@GroupConfigurationItem' {} a -> s {parameters = a} :: GroupConfigurationItem) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the type of group configuration item. Each item must have a
-- unique value for @type@. For the list of types that you can specify for
-- a configuration item, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html#about-slg-types Supported resource types and parameters>.
groupConfigurationItem_type :: Lens.Lens' GroupConfigurationItem Prelude.Text
groupConfigurationItem_type = Lens.lens (\GroupConfigurationItem' {type'} -> type') (\s@GroupConfigurationItem' {} a -> s {type' = a} :: GroupConfigurationItem)

instance Prelude.FromJSON GroupConfigurationItem where
  parseJSON =
    Prelude.withObject
      "GroupConfigurationItem"
      ( \x ->
          GroupConfigurationItem'
            Prelude.<$> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable GroupConfigurationItem

instance Prelude.NFData GroupConfigurationItem

instance Prelude.ToJSON GroupConfigurationItem where
  toJSON GroupConfigurationItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Parameters" Prelude..=) Prelude.<$> parameters,
            Prelude.Just ("Type" Prelude..= type')
          ]
      )

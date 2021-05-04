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
-- Module      : Network.AWS.Greengrass.Types.GroupOwnerSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupOwnerSetting where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Group owner related settings for local resources.
--
-- /See:/ 'newGroupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { -- | The name of the Linux OS group whose privileges will be added to the
    -- Lambda process. This field is optional.
    groupOwner :: Prelude.Maybe Prelude.Text,
    -- | If true, AWS IoT Greengrass automatically adds the specified Linux OS
    -- group owner of the resource to the Lambda process privileges. Thus the
    -- Lambda process will have the file access permissions of the added Linux
    -- group.
    autoAddGroupOwner :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GroupOwnerSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupOwner', 'groupOwnerSetting_groupOwner' - The name of the Linux OS group whose privileges will be added to the
-- Lambda process. This field is optional.
--
-- 'autoAddGroupOwner', 'groupOwnerSetting_autoAddGroupOwner' - If true, AWS IoT Greengrass automatically adds the specified Linux OS
-- group owner of the resource to the Lambda process privileges. Thus the
-- Lambda process will have the file access permissions of the added Linux
-- group.
newGroupOwnerSetting ::
  GroupOwnerSetting
newGroupOwnerSetting =
  GroupOwnerSetting'
    { groupOwner = Prelude.Nothing,
      autoAddGroupOwner = Prelude.Nothing
    }

-- | The name of the Linux OS group whose privileges will be added to the
-- Lambda process. This field is optional.
groupOwnerSetting_groupOwner :: Lens.Lens' GroupOwnerSetting (Prelude.Maybe Prelude.Text)
groupOwnerSetting_groupOwner = Lens.lens (\GroupOwnerSetting' {groupOwner} -> groupOwner) (\s@GroupOwnerSetting' {} a -> s {groupOwner = a} :: GroupOwnerSetting)

-- | If true, AWS IoT Greengrass automatically adds the specified Linux OS
-- group owner of the resource to the Lambda process privileges. Thus the
-- Lambda process will have the file access permissions of the added Linux
-- group.
groupOwnerSetting_autoAddGroupOwner :: Lens.Lens' GroupOwnerSetting (Prelude.Maybe Prelude.Bool)
groupOwnerSetting_autoAddGroupOwner = Lens.lens (\GroupOwnerSetting' {autoAddGroupOwner} -> autoAddGroupOwner) (\s@GroupOwnerSetting' {} a -> s {autoAddGroupOwner = a} :: GroupOwnerSetting)

instance Prelude.FromJSON GroupOwnerSetting where
  parseJSON =
    Prelude.withObject
      "GroupOwnerSetting"
      ( \x ->
          GroupOwnerSetting'
            Prelude.<$> (x Prelude..:? "GroupOwner")
            Prelude.<*> (x Prelude..:? "AutoAddGroupOwner")
      )

instance Prelude.Hashable GroupOwnerSetting

instance Prelude.NFData GroupOwnerSetting

instance Prelude.ToJSON GroupOwnerSetting where
  toJSON GroupOwnerSetting' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GroupOwner" Prelude..=) Prelude.<$> groupOwner,
            ("AutoAddGroupOwner" Prelude..=)
              Prelude.<$> autoAddGroupOwner
          ]
      )

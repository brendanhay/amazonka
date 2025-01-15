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
-- Module      : Amazonka.Greengrass.Types.GroupOwnerSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.GroupOwnerSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Group owner related settings for local resources.
--
-- /See:/ 'newGroupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { -- | If true, AWS IoT Greengrass automatically adds the specified Linux OS
    -- group owner of the resource to the Lambda process privileges. Thus the
    -- Lambda process will have the file access permissions of the added Linux
    -- group.
    autoAddGroupOwner :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Linux OS group whose privileges will be added to the
    -- Lambda process. This field is optional.
    groupOwner :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupOwnerSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoAddGroupOwner', 'groupOwnerSetting_autoAddGroupOwner' - If true, AWS IoT Greengrass automatically adds the specified Linux OS
-- group owner of the resource to the Lambda process privileges. Thus the
-- Lambda process will have the file access permissions of the added Linux
-- group.
--
-- 'groupOwner', 'groupOwnerSetting_groupOwner' - The name of the Linux OS group whose privileges will be added to the
-- Lambda process. This field is optional.
newGroupOwnerSetting ::
  GroupOwnerSetting
newGroupOwnerSetting =
  GroupOwnerSetting'
    { autoAddGroupOwner =
        Prelude.Nothing,
      groupOwner = Prelude.Nothing
    }

-- | If true, AWS IoT Greengrass automatically adds the specified Linux OS
-- group owner of the resource to the Lambda process privileges. Thus the
-- Lambda process will have the file access permissions of the added Linux
-- group.
groupOwnerSetting_autoAddGroupOwner :: Lens.Lens' GroupOwnerSetting (Prelude.Maybe Prelude.Bool)
groupOwnerSetting_autoAddGroupOwner = Lens.lens (\GroupOwnerSetting' {autoAddGroupOwner} -> autoAddGroupOwner) (\s@GroupOwnerSetting' {} a -> s {autoAddGroupOwner = a} :: GroupOwnerSetting)

-- | The name of the Linux OS group whose privileges will be added to the
-- Lambda process. This field is optional.
groupOwnerSetting_groupOwner :: Lens.Lens' GroupOwnerSetting (Prelude.Maybe Prelude.Text)
groupOwnerSetting_groupOwner = Lens.lens (\GroupOwnerSetting' {groupOwner} -> groupOwner) (\s@GroupOwnerSetting' {} a -> s {groupOwner = a} :: GroupOwnerSetting)

instance Data.FromJSON GroupOwnerSetting where
  parseJSON =
    Data.withObject
      "GroupOwnerSetting"
      ( \x ->
          GroupOwnerSetting'
            Prelude.<$> (x Data..:? "AutoAddGroupOwner")
            Prelude.<*> (x Data..:? "GroupOwner")
      )

instance Prelude.Hashable GroupOwnerSetting where
  hashWithSalt _salt GroupOwnerSetting' {..} =
    _salt
      `Prelude.hashWithSalt` autoAddGroupOwner
      `Prelude.hashWithSalt` groupOwner

instance Prelude.NFData GroupOwnerSetting where
  rnf GroupOwnerSetting' {..} =
    Prelude.rnf autoAddGroupOwner `Prelude.seq`
      Prelude.rnf groupOwner

instance Data.ToJSON GroupOwnerSetting where
  toJSON GroupOwnerSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoAddGroupOwner" Data..=)
              Prelude.<$> autoAddGroupOwner,
            ("GroupOwner" Data..=) Prelude.<$> groupOwner
          ]
      )

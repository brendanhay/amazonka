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
-- Module      : Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting where

import Network.AWS.Greengrass.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The owner setting for downloaded machine learning resources.
--
-- /See:/ 'newResourceDownloadOwnerSetting' smart constructor.
data ResourceDownloadOwnerSetting = ResourceDownloadOwnerSetting'
  { -- | The group owner of the resource. This is the name of an existing Linux
    -- OS group on the system or a GID. The group\'s permissions are added to
    -- the Lambda process.
    groupOwner :: Prelude.Text,
    -- | The permissions that the group owner has to the resource. Valid values
    -- are \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
    groupPermission :: Permission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDownloadOwnerSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupOwner', 'resourceDownloadOwnerSetting_groupOwner' - The group owner of the resource. This is the name of an existing Linux
-- OS group on the system or a GID. The group\'s permissions are added to
-- the Lambda process.
--
-- 'groupPermission', 'resourceDownloadOwnerSetting_groupPermission' - The permissions that the group owner has to the resource. Valid values
-- are \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
newResourceDownloadOwnerSetting ::
  -- | 'groupOwner'
  Prelude.Text ->
  -- | 'groupPermission'
  Permission ->
  ResourceDownloadOwnerSetting
newResourceDownloadOwnerSetting
  pGroupOwner_
  pGroupPermission_ =
    ResourceDownloadOwnerSetting'
      { groupOwner =
          pGroupOwner_,
        groupPermission = pGroupPermission_
      }

-- | The group owner of the resource. This is the name of an existing Linux
-- OS group on the system or a GID. The group\'s permissions are added to
-- the Lambda process.
resourceDownloadOwnerSetting_groupOwner :: Lens.Lens' ResourceDownloadOwnerSetting Prelude.Text
resourceDownloadOwnerSetting_groupOwner = Lens.lens (\ResourceDownloadOwnerSetting' {groupOwner} -> groupOwner) (\s@ResourceDownloadOwnerSetting' {} a -> s {groupOwner = a} :: ResourceDownloadOwnerSetting)

-- | The permissions that the group owner has to the resource. Valid values
-- are \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
resourceDownloadOwnerSetting_groupPermission :: Lens.Lens' ResourceDownloadOwnerSetting Permission
resourceDownloadOwnerSetting_groupPermission = Lens.lens (\ResourceDownloadOwnerSetting' {groupPermission} -> groupPermission) (\s@ResourceDownloadOwnerSetting' {} a -> s {groupPermission = a} :: ResourceDownloadOwnerSetting)

instance
  Prelude.FromJSON
    ResourceDownloadOwnerSetting
  where
  parseJSON =
    Prelude.withObject
      "ResourceDownloadOwnerSetting"
      ( \x ->
          ResourceDownloadOwnerSetting'
            Prelude.<$> (x Prelude..: "GroupOwner")
            Prelude.<*> (x Prelude..: "GroupPermission")
      )

instance
  Prelude.Hashable
    ResourceDownloadOwnerSetting

instance Prelude.NFData ResourceDownloadOwnerSetting

instance Prelude.ToJSON ResourceDownloadOwnerSetting where
  toJSON ResourceDownloadOwnerSetting' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupOwner" Prelude..= groupOwner),
            Prelude.Just
              ("GroupPermission" Prelude..= groupPermission)
          ]
      )

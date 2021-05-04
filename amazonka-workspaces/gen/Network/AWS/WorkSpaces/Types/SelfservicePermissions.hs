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
-- Module      : Network.AWS.WorkSpaces.Types.SelfservicePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.SelfservicePermissions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes the self-service permissions for a directory. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users>.
--
-- /See:/ 'newSelfservicePermissions' smart constructor.
data SelfservicePermissions = SelfservicePermissions'
  { -- | Specifies whether users can switch the running mode of their WorkSpace.
    switchRunningMode :: Prelude.Maybe ReconnectEnum,
    -- | Specifies whether users can restart their WorkSpace.
    restartWorkspace :: Prelude.Maybe ReconnectEnum,
    -- | Specifies whether users can rebuild the operating system of a WorkSpace
    -- to its original state.
    rebuildWorkspace :: Prelude.Maybe ReconnectEnum,
    -- | Specifies whether users can increase the volume size of the drives on
    -- their WorkSpace.
    increaseVolumeSize :: Prelude.Maybe ReconnectEnum,
    -- | Specifies whether users can change the compute type (bundle) for their
    -- WorkSpace.
    changeComputeType :: Prelude.Maybe ReconnectEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SelfservicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'switchRunningMode', 'selfservicePermissions_switchRunningMode' - Specifies whether users can switch the running mode of their WorkSpace.
--
-- 'restartWorkspace', 'selfservicePermissions_restartWorkspace' - Specifies whether users can restart their WorkSpace.
--
-- 'rebuildWorkspace', 'selfservicePermissions_rebuildWorkspace' - Specifies whether users can rebuild the operating system of a WorkSpace
-- to its original state.
--
-- 'increaseVolumeSize', 'selfservicePermissions_increaseVolumeSize' - Specifies whether users can increase the volume size of the drives on
-- their WorkSpace.
--
-- 'changeComputeType', 'selfservicePermissions_changeComputeType' - Specifies whether users can change the compute type (bundle) for their
-- WorkSpace.
newSelfservicePermissions ::
  SelfservicePermissions
newSelfservicePermissions =
  SelfservicePermissions'
    { switchRunningMode =
        Prelude.Nothing,
      restartWorkspace = Prelude.Nothing,
      rebuildWorkspace = Prelude.Nothing,
      increaseVolumeSize = Prelude.Nothing,
      changeComputeType = Prelude.Nothing
    }

-- | Specifies whether users can switch the running mode of their WorkSpace.
selfservicePermissions_switchRunningMode :: Lens.Lens' SelfservicePermissions (Prelude.Maybe ReconnectEnum)
selfservicePermissions_switchRunningMode = Lens.lens (\SelfservicePermissions' {switchRunningMode} -> switchRunningMode) (\s@SelfservicePermissions' {} a -> s {switchRunningMode = a} :: SelfservicePermissions)

-- | Specifies whether users can restart their WorkSpace.
selfservicePermissions_restartWorkspace :: Lens.Lens' SelfservicePermissions (Prelude.Maybe ReconnectEnum)
selfservicePermissions_restartWorkspace = Lens.lens (\SelfservicePermissions' {restartWorkspace} -> restartWorkspace) (\s@SelfservicePermissions' {} a -> s {restartWorkspace = a} :: SelfservicePermissions)

-- | Specifies whether users can rebuild the operating system of a WorkSpace
-- to its original state.
selfservicePermissions_rebuildWorkspace :: Lens.Lens' SelfservicePermissions (Prelude.Maybe ReconnectEnum)
selfservicePermissions_rebuildWorkspace = Lens.lens (\SelfservicePermissions' {rebuildWorkspace} -> rebuildWorkspace) (\s@SelfservicePermissions' {} a -> s {rebuildWorkspace = a} :: SelfservicePermissions)

-- | Specifies whether users can increase the volume size of the drives on
-- their WorkSpace.
selfservicePermissions_increaseVolumeSize :: Lens.Lens' SelfservicePermissions (Prelude.Maybe ReconnectEnum)
selfservicePermissions_increaseVolumeSize = Lens.lens (\SelfservicePermissions' {increaseVolumeSize} -> increaseVolumeSize) (\s@SelfservicePermissions' {} a -> s {increaseVolumeSize = a} :: SelfservicePermissions)

-- | Specifies whether users can change the compute type (bundle) for their
-- WorkSpace.
selfservicePermissions_changeComputeType :: Lens.Lens' SelfservicePermissions (Prelude.Maybe ReconnectEnum)
selfservicePermissions_changeComputeType = Lens.lens (\SelfservicePermissions' {changeComputeType} -> changeComputeType) (\s@SelfservicePermissions' {} a -> s {changeComputeType = a} :: SelfservicePermissions)

instance Prelude.FromJSON SelfservicePermissions where
  parseJSON =
    Prelude.withObject
      "SelfservicePermissions"
      ( \x ->
          SelfservicePermissions'
            Prelude.<$> (x Prelude..:? "SwitchRunningMode")
            Prelude.<*> (x Prelude..:? "RestartWorkspace")
            Prelude.<*> (x Prelude..:? "RebuildWorkspace")
            Prelude.<*> (x Prelude..:? "IncreaseVolumeSize")
            Prelude.<*> (x Prelude..:? "ChangeComputeType")
      )

instance Prelude.Hashable SelfservicePermissions

instance Prelude.NFData SelfservicePermissions

instance Prelude.ToJSON SelfservicePermissions where
  toJSON SelfservicePermissions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SwitchRunningMode" Prelude..=)
              Prelude.<$> switchRunningMode,
            ("RestartWorkspace" Prelude..=)
              Prelude.<$> restartWorkspace,
            ("RebuildWorkspace" Prelude..=)
              Prelude.<$> rebuildWorkspace,
            ("IncreaseVolumeSize" Prelude..=)
              Prelude.<$> increaseVolumeSize,
            ("ChangeComputeType" Prelude..=)
              Prelude.<$> changeComputeType
          ]
      )

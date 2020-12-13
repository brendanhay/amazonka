{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.SelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.SelfservicePermissions
  ( SelfservicePermissions (..),

    -- * Smart constructor
    mkSelfservicePermissions,

    -- * Lenses
    spRestartWorkspace,
    spChangeComputeType,
    spSwitchRunningMode,
    spRebuildWorkspace,
    spIncreaseVolumeSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes the self-service permissions for a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
--
-- /See:/ 'mkSelfservicePermissions' smart constructor.
data SelfservicePermissions = SelfservicePermissions'
  { -- | Specifies whether users can restart their WorkSpace.
    restartWorkspace :: Lude.Maybe ReconnectEnum,
    -- | Specifies whether users can change the compute type (bundle) for their WorkSpace.
    changeComputeType :: Lude.Maybe ReconnectEnum,
    -- | Specifies whether users can switch the running mode of their WorkSpace.
    switchRunningMode :: Lude.Maybe ReconnectEnum,
    -- | Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
    rebuildWorkspace :: Lude.Maybe ReconnectEnum,
    -- | Specifies whether users can increase the volume size of the drives on their WorkSpace.
    increaseVolumeSize :: Lude.Maybe ReconnectEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelfservicePermissions' with the minimum fields required to make a request.
--
-- * 'restartWorkspace' - Specifies whether users can restart their WorkSpace.
-- * 'changeComputeType' - Specifies whether users can change the compute type (bundle) for their WorkSpace.
-- * 'switchRunningMode' - Specifies whether users can switch the running mode of their WorkSpace.
-- * 'rebuildWorkspace' - Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
-- * 'increaseVolumeSize' - Specifies whether users can increase the volume size of the drives on their WorkSpace.
mkSelfservicePermissions ::
  SelfservicePermissions
mkSelfservicePermissions =
  SelfservicePermissions'
    { restartWorkspace = Lude.Nothing,
      changeComputeType = Lude.Nothing,
      switchRunningMode = Lude.Nothing,
      rebuildWorkspace = Lude.Nothing,
      increaseVolumeSize = Lude.Nothing
    }

-- | Specifies whether users can restart their WorkSpace.
--
-- /Note:/ Consider using 'restartWorkspace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRestartWorkspace :: Lens.Lens' SelfservicePermissions (Lude.Maybe ReconnectEnum)
spRestartWorkspace = Lens.lens (restartWorkspace :: SelfservicePermissions -> Lude.Maybe ReconnectEnum) (\s a -> s {restartWorkspace = a} :: SelfservicePermissions)
{-# DEPRECATED spRestartWorkspace "Use generic-lens or generic-optics with 'restartWorkspace' instead." #-}

-- | Specifies whether users can change the compute type (bundle) for their WorkSpace.
--
-- /Note:/ Consider using 'changeComputeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spChangeComputeType :: Lens.Lens' SelfservicePermissions (Lude.Maybe ReconnectEnum)
spChangeComputeType = Lens.lens (changeComputeType :: SelfservicePermissions -> Lude.Maybe ReconnectEnum) (\s a -> s {changeComputeType = a} :: SelfservicePermissions)
{-# DEPRECATED spChangeComputeType "Use generic-lens or generic-optics with 'changeComputeType' instead." #-}

-- | Specifies whether users can switch the running mode of their WorkSpace.
--
-- /Note:/ Consider using 'switchRunningMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSwitchRunningMode :: Lens.Lens' SelfservicePermissions (Lude.Maybe ReconnectEnum)
spSwitchRunningMode = Lens.lens (switchRunningMode :: SelfservicePermissions -> Lude.Maybe ReconnectEnum) (\s a -> s {switchRunningMode = a} :: SelfservicePermissions)
{-# DEPRECATED spSwitchRunningMode "Use generic-lens or generic-optics with 'switchRunningMode' instead." #-}

-- | Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
--
-- /Note:/ Consider using 'rebuildWorkspace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRebuildWorkspace :: Lens.Lens' SelfservicePermissions (Lude.Maybe ReconnectEnum)
spRebuildWorkspace = Lens.lens (rebuildWorkspace :: SelfservicePermissions -> Lude.Maybe ReconnectEnum) (\s a -> s {rebuildWorkspace = a} :: SelfservicePermissions)
{-# DEPRECATED spRebuildWorkspace "Use generic-lens or generic-optics with 'rebuildWorkspace' instead." #-}

-- | Specifies whether users can increase the volume size of the drives on their WorkSpace.
--
-- /Note:/ Consider using 'increaseVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spIncreaseVolumeSize :: Lens.Lens' SelfservicePermissions (Lude.Maybe ReconnectEnum)
spIncreaseVolumeSize = Lens.lens (increaseVolumeSize :: SelfservicePermissions -> Lude.Maybe ReconnectEnum) (\s a -> s {increaseVolumeSize = a} :: SelfservicePermissions)
{-# DEPRECATED spIncreaseVolumeSize "Use generic-lens or generic-optics with 'increaseVolumeSize' instead." #-}

instance Lude.FromJSON SelfservicePermissions where
  parseJSON =
    Lude.withObject
      "SelfservicePermissions"
      ( \x ->
          SelfservicePermissions'
            Lude.<$> (x Lude..:? "RestartWorkspace")
            Lude.<*> (x Lude..:? "ChangeComputeType")
            Lude.<*> (x Lude..:? "SwitchRunningMode")
            Lude.<*> (x Lude..:? "RebuildWorkspace")
            Lude.<*> (x Lude..:? "IncreaseVolumeSize")
      )

instance Lude.ToJSON SelfservicePermissions where
  toJSON SelfservicePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RestartWorkspace" Lude..=) Lude.<$> restartWorkspace,
            ("ChangeComputeType" Lude..=) Lude.<$> changeComputeType,
            ("SwitchRunningMode" Lude..=) Lude.<$> switchRunningMode,
            ("RebuildWorkspace" Lude..=) Lude.<$> rebuildWorkspace,
            ("IncreaseVolumeSize" Lude..=) Lude.<$> increaseVolumeSize
          ]
      )

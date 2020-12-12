{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VMServer
  ( VMServer (..),

    -- * Smart constructor
    mkVMServer,

    -- * Lenses
    vmsVmManagerName,
    vmsVmManagerType,
    vmsVmServerAddress,
    vmsVmName,
    vmsVmPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.VMManagerType
import Network.AWS.SMS.Types.VMServerAddress

-- | Represents a VM server.
--
-- /See:/ 'mkVMServer' smart constructor.
data VMServer = VMServer'
  { vmManagerName :: Lude.Maybe Lude.Text,
    vmManagerType :: Lude.Maybe VMManagerType,
    vmServerAddress :: Lude.Maybe VMServerAddress,
    vmName :: Lude.Maybe Lude.Text,
    vmPath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VMServer' with the minimum fields required to make a request.
--
-- * 'vmManagerName' - The name of the VM manager.
-- * 'vmManagerType' - The type of VM management product.
-- * 'vmName' - The name of the VM.
-- * 'vmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
-- * 'vmServerAddress' - The VM server location.
mkVMServer ::
  VMServer
mkVMServer =
  VMServer'
    { vmManagerName = Lude.Nothing,
      vmManagerType = Lude.Nothing,
      vmServerAddress = Lude.Nothing,
      vmName = Lude.Nothing,
      vmPath = Lude.Nothing
    }

-- | The name of the VM manager.
--
-- /Note:/ Consider using 'vmManagerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsVmManagerName :: Lens.Lens' VMServer (Lude.Maybe Lude.Text)
vmsVmManagerName = Lens.lens (vmManagerName :: VMServer -> Lude.Maybe Lude.Text) (\s a -> s {vmManagerName = a} :: VMServer)
{-# DEPRECATED vmsVmManagerName "Use generic-lens or generic-optics with 'vmManagerName' instead." #-}

-- | The type of VM management product.
--
-- /Note:/ Consider using 'vmManagerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsVmManagerType :: Lens.Lens' VMServer (Lude.Maybe VMManagerType)
vmsVmManagerType = Lens.lens (vmManagerType :: VMServer -> Lude.Maybe VMManagerType) (\s a -> s {vmManagerType = a} :: VMServer)
{-# DEPRECATED vmsVmManagerType "Use generic-lens or generic-optics with 'vmManagerType' instead." #-}

-- | The VM server location.
--
-- /Note:/ Consider using 'vmServerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsVmServerAddress :: Lens.Lens' VMServer (Lude.Maybe VMServerAddress)
vmsVmServerAddress = Lens.lens (vmServerAddress :: VMServer -> Lude.Maybe VMServerAddress) (\s a -> s {vmServerAddress = a} :: VMServer)
{-# DEPRECATED vmsVmServerAddress "Use generic-lens or generic-optics with 'vmServerAddress' instead." #-}

-- | The name of the VM.
--
-- /Note:/ Consider using 'vmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsVmName :: Lens.Lens' VMServer (Lude.Maybe Lude.Text)
vmsVmName = Lens.lens (vmName :: VMServer -> Lude.Maybe Lude.Text) (\s a -> s {vmName = a} :: VMServer)
{-# DEPRECATED vmsVmName "Use generic-lens or generic-optics with 'vmName' instead." #-}

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
--
-- /Note:/ Consider using 'vmPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsVmPath :: Lens.Lens' VMServer (Lude.Maybe Lude.Text)
vmsVmPath = Lens.lens (vmPath :: VMServer -> Lude.Maybe Lude.Text) (\s a -> s {vmPath = a} :: VMServer)
{-# DEPRECATED vmsVmPath "Use generic-lens or generic-optics with 'vmPath' instead." #-}

instance Lude.FromJSON VMServer where
  parseJSON =
    Lude.withObject
      "VMServer"
      ( \x ->
          VMServer'
            Lude.<$> (x Lude..:? "vmManagerName")
            Lude.<*> (x Lude..:? "vmManagerType")
            Lude.<*> (x Lude..:? "vmServerAddress")
            Lude.<*> (x Lude..:? "vmName")
            Lude.<*> (x Lude..:? "vmPath")
      )

instance Lude.ToJSON VMServer where
  toJSON VMServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("vmManagerName" Lude..=) Lude.<$> vmManagerName,
            ("vmManagerType" Lude..=) Lude.<$> vmManagerType,
            ("vmServerAddress" Lude..=) Lude.<$> vmServerAddress,
            ("vmName" Lude..=) Lude.<$> vmName,
            ("vmPath" Lude..=) Lude.<$> vmPath
          ]
      )

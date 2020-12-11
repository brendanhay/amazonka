-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMServerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VMServerAddress
  ( VMServerAddress (..),

    -- * Smart constructor
    mkVMServerAddress,

    -- * Lenses
    vmsaVmManagerId,
    vmsaVmId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a VM server location.
--
-- /See:/ 'mkVMServerAddress' smart constructor.
data VMServerAddress = VMServerAddress'
  { vmManagerId ::
      Lude.Maybe Lude.Text,
    vmId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VMServerAddress' with the minimum fields required to make a request.
--
-- * 'vmId' - The ID of the VM.
-- * 'vmManagerId' - The ID of the VM manager.
mkVMServerAddress ::
  VMServerAddress
mkVMServerAddress =
  VMServerAddress' {vmManagerId = Lude.Nothing, vmId = Lude.Nothing}

-- | The ID of the VM manager.
--
-- /Note:/ Consider using 'vmManagerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsaVmManagerId :: Lens.Lens' VMServerAddress (Lude.Maybe Lude.Text)
vmsaVmManagerId = Lens.lens (vmManagerId :: VMServerAddress -> Lude.Maybe Lude.Text) (\s a -> s {vmManagerId = a} :: VMServerAddress)
{-# DEPRECATED vmsaVmManagerId "Use generic-lens or generic-optics with 'vmManagerId' instead." #-}

-- | The ID of the VM.
--
-- /Note:/ Consider using 'vmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmsaVmId :: Lens.Lens' VMServerAddress (Lude.Maybe Lude.Text)
vmsaVmId = Lens.lens (vmId :: VMServerAddress -> Lude.Maybe Lude.Text) (\s a -> s {vmId = a} :: VMServerAddress)
{-# DEPRECATED vmsaVmId "Use generic-lens or generic-optics with 'vmId' instead." #-}

instance Lude.FromJSON VMServerAddress where
  parseJSON =
    Lude.withObject
      "VMServerAddress"
      ( \x ->
          VMServerAddress'
            Lude.<$> (x Lude..:? "vmManagerId") Lude.<*> (x Lude..:? "vmId")
      )

instance Lude.ToJSON VMServerAddress where
  toJSON VMServerAddress' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("vmManagerId" Lude..=) Lude.<$> vmManagerId,
            ("vmId" Lude..=) Lude.<$> vmId
          ]
      )

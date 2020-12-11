-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EBSResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EBSResourceUtilization
  ( EBSResourceUtilization (..),

    -- * Smart constructor
    mkEBSResourceUtilization,

    -- * Lenses
    eruEBSWriteBytesPerSecond,
    eruEBSWriteOpsPerSecond,
    eruEBSReadOpsPerSecond,
    eruEBSReadBytesPerSecond,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The EBS field that contains a list of EBS metrics associated with the current instance.
--
-- /See:/ 'mkEBSResourceUtilization' smart constructor.
data EBSResourceUtilization = EBSResourceUtilization'
  { ebsWriteBytesPerSecond ::
      Lude.Maybe Lude.Text,
    ebsWriteOpsPerSecond :: Lude.Maybe Lude.Text,
    ebsReadOpsPerSecond :: Lude.Maybe Lude.Text,
    ebsReadBytesPerSecond :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSResourceUtilization' with the minimum fields required to make a request.
--
-- * 'ebsReadBytesPerSecond' - The maximum size of read operations per second
-- * 'ebsReadOpsPerSecond' - The maximum number of read operations per second.
-- * 'ebsWriteBytesPerSecond' - The maximum size of write operations per second.
-- * 'ebsWriteOpsPerSecond' - The maximum number of write operations per second.
mkEBSResourceUtilization ::
  EBSResourceUtilization
mkEBSResourceUtilization =
  EBSResourceUtilization'
    { ebsWriteBytesPerSecond = Lude.Nothing,
      ebsWriteOpsPerSecond = Lude.Nothing,
      ebsReadOpsPerSecond = Lude.Nothing,
      ebsReadBytesPerSecond = Lude.Nothing
    }

-- | The maximum size of write operations per second.
--
-- /Note:/ Consider using 'ebsWriteBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruEBSWriteBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Lude.Maybe Lude.Text)
eruEBSWriteBytesPerSecond = Lens.lens (ebsWriteBytesPerSecond :: EBSResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {ebsWriteBytesPerSecond = a} :: EBSResourceUtilization)
{-# DEPRECATED eruEBSWriteBytesPerSecond "Use generic-lens or generic-optics with 'ebsWriteBytesPerSecond' instead." #-}

-- | The maximum number of write operations per second.
--
-- /Note:/ Consider using 'ebsWriteOpsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruEBSWriteOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Lude.Maybe Lude.Text)
eruEBSWriteOpsPerSecond = Lens.lens (ebsWriteOpsPerSecond :: EBSResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {ebsWriteOpsPerSecond = a} :: EBSResourceUtilization)
{-# DEPRECATED eruEBSWriteOpsPerSecond "Use generic-lens or generic-optics with 'ebsWriteOpsPerSecond' instead." #-}

-- | The maximum number of read operations per second.
--
-- /Note:/ Consider using 'ebsReadOpsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruEBSReadOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Lude.Maybe Lude.Text)
eruEBSReadOpsPerSecond = Lens.lens (ebsReadOpsPerSecond :: EBSResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {ebsReadOpsPerSecond = a} :: EBSResourceUtilization)
{-# DEPRECATED eruEBSReadOpsPerSecond "Use generic-lens or generic-optics with 'ebsReadOpsPerSecond' instead." #-}

-- | The maximum size of read operations per second
--
-- /Note:/ Consider using 'ebsReadBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruEBSReadBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Lude.Maybe Lude.Text)
eruEBSReadBytesPerSecond = Lens.lens (ebsReadBytesPerSecond :: EBSResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {ebsReadBytesPerSecond = a} :: EBSResourceUtilization)
{-# DEPRECATED eruEBSReadBytesPerSecond "Use generic-lens or generic-optics with 'ebsReadBytesPerSecond' instead." #-}

instance Lude.FromJSON EBSResourceUtilization where
  parseJSON =
    Lude.withObject
      "EBSResourceUtilization"
      ( \x ->
          EBSResourceUtilization'
            Lude.<$> (x Lude..:? "EbsWriteBytesPerSecond")
            Lude.<*> (x Lude..:? "EbsWriteOpsPerSecond")
            Lude.<*> (x Lude..:? "EbsReadOpsPerSecond")
            Lude.<*> (x Lude..:? "EbsReadBytesPerSecond")
      )

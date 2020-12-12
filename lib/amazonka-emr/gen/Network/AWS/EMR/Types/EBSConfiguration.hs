{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSConfiguration
  ( EBSConfiguration (..),

    -- * Smart constructor
    mkEBSConfiguration,

    -- * Lenses
    ecEBSOptimized,
    ecEBSBlockDeviceConfigs,
  )
where

import Network.AWS.EMR.Types.EBSBlockDeviceConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon EBS configuration of a cluster instance.
--
-- /See:/ 'mkEBSConfiguration' smart constructor.
data EBSConfiguration = EBSConfiguration'
  { ebsOptimized ::
      Lude.Maybe Lude.Bool,
    ebsBlockDeviceConfigs ::
      Lude.Maybe [EBSBlockDeviceConfig]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSConfiguration' with the minimum fields required to make a request.
--
-- * 'ebsBlockDeviceConfigs' - An array of Amazon EBS volume specifications attached to a cluster instance.
-- * 'ebsOptimized' - Indicates whether an Amazon EBS volume is EBS-optimized.
mkEBSConfiguration ::
  EBSConfiguration
mkEBSConfiguration =
  EBSConfiguration'
    { ebsOptimized = Lude.Nothing,
      ebsBlockDeviceConfigs = Lude.Nothing
    }

-- | Indicates whether an Amazon EBS volume is EBS-optimized.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEBSOptimized :: Lens.Lens' EBSConfiguration (Lude.Maybe Lude.Bool)
ecEBSOptimized = Lens.lens (ebsOptimized :: EBSConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: EBSConfiguration)
{-# DEPRECATED ecEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | An array of Amazon EBS volume specifications attached to a cluster instance.
--
-- /Note:/ Consider using 'ebsBlockDeviceConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEBSBlockDeviceConfigs :: Lens.Lens' EBSConfiguration (Lude.Maybe [EBSBlockDeviceConfig])
ecEBSBlockDeviceConfigs = Lens.lens (ebsBlockDeviceConfigs :: EBSConfiguration -> Lude.Maybe [EBSBlockDeviceConfig]) (\s a -> s {ebsBlockDeviceConfigs = a} :: EBSConfiguration)
{-# DEPRECATED ecEBSBlockDeviceConfigs "Use generic-lens or generic-optics with 'ebsBlockDeviceConfigs' instead." #-}

instance Lude.ToJSON EBSConfiguration where
  toJSON EBSConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EbsOptimized" Lude..=) Lude.<$> ebsOptimized,
            ("EbsBlockDeviceConfigs" Lude..=) Lude.<$> ebsBlockDeviceConfigs
          ]
      )

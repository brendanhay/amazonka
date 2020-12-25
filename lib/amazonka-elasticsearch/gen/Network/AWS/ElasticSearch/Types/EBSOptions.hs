{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptions
  ( EBSOptions (..),

    -- * Smart constructor
    mkEBSOptions,

    -- * Lenses
    ebsoEBSEnabled,
    ebsoIops,
    ebsoVolumeSize,
    ebsoVolumeType,
  )
where

import qualified Network.AWS.ElasticSearch.Types.VolumeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> .
--
-- /See:/ 'mkEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Specifies whether EBS-based storage is enabled.
    eBSEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
    iops :: Core.Maybe Core.Int,
    -- | Integer to specify the size of an EBS volume.
    volumeSize :: Core.Maybe Core.Int,
    -- | Specifies the volume type for EBS-based storage.
    volumeType :: Core.Maybe Types.VolumeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EBSOptions' value with any optional fields omitted.
mkEBSOptions ::
  EBSOptions
mkEBSOptions =
  EBSOptions'
    { eBSEnabled = Core.Nothing,
      iops = Core.Nothing,
      volumeSize = Core.Nothing,
      volumeType = Core.Nothing
    }

-- | Specifies whether EBS-based storage is enabled.
--
-- /Note:/ Consider using 'eBSEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsoEBSEnabled :: Lens.Lens' EBSOptions (Core.Maybe Core.Bool)
ebsoEBSEnabled = Lens.field @"eBSEnabled"
{-# DEPRECATED ebsoEBSEnabled "Use generic-lens or generic-optics with 'eBSEnabled' instead." #-}

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsoIops :: Lens.Lens' EBSOptions (Core.Maybe Core.Int)
ebsoIops = Lens.field @"iops"
{-# DEPRECATED ebsoIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Integer to specify the size of an EBS volume.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsoVolumeSize :: Lens.Lens' EBSOptions (Core.Maybe Core.Int)
ebsoVolumeSize = Lens.field @"volumeSize"
{-# DEPRECATED ebsoVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | Specifies the volume type for EBS-based storage.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsoVolumeType :: Lens.Lens' EBSOptions (Core.Maybe Types.VolumeType)
ebsoVolumeType = Lens.field @"volumeType"
{-# DEPRECATED ebsoVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Core.FromJSON EBSOptions where
  toJSON EBSOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("EBSEnabled" Core..=) Core.<$> eBSEnabled,
            ("Iops" Core..=) Core.<$> iops,
            ("VolumeSize" Core..=) Core.<$> volumeSize,
            ("VolumeType" Core..=) Core.<$> volumeType
          ]
      )

instance Core.FromJSON EBSOptions where
  parseJSON =
    Core.withObject "EBSOptions" Core.$
      \x ->
        EBSOptions'
          Core.<$> (x Core..:? "EBSEnabled")
          Core.<*> (x Core..:? "Iops")
          Core.<*> (x Core..:? "VolumeSize")
          Core.<*> (x Core..:? "VolumeType")

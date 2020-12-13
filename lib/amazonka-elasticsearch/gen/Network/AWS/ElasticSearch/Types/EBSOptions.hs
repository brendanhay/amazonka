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
    eoVolumeSize,
    eoIOPS,
    eoVolumeType,
    eoEBSEnabled,
  )
where

import Network.AWS.ElasticSearch.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> .
--
-- /See:/ 'mkEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Integer to specify the size of an EBS volume.
    volumeSize :: Lude.Maybe Lude.Int,
    -- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
    iops :: Lude.Maybe Lude.Int,
    -- | Specifies the volume type for EBS-based storage.
    volumeType :: Lude.Maybe VolumeType,
    -- | Specifies whether EBS-based storage is enabled.
    ebsEnabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSOptions' with the minimum fields required to make a request.
--
-- * 'volumeSize' - Integer to specify the size of an EBS volume.
-- * 'iops' - Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
-- * 'volumeType' - Specifies the volume type for EBS-based storage.
-- * 'ebsEnabled' - Specifies whether EBS-based storage is enabled.
mkEBSOptions ::
  EBSOptions
mkEBSOptions =
  EBSOptions'
    { volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      volumeType = Lude.Nothing,
      ebsEnabled = Lude.Nothing
    }

-- | Integer to specify the size of an EBS volume.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoVolumeSize :: Lens.Lens' EBSOptions (Lude.Maybe Lude.Int)
eoVolumeSize = Lens.lens (volumeSize :: EBSOptions -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: EBSOptions)
{-# DEPRECATED eoVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoIOPS :: Lens.Lens' EBSOptions (Lude.Maybe Lude.Int)
eoIOPS = Lens.lens (iops :: EBSOptions -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: EBSOptions)
{-# DEPRECATED eoIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Specifies the volume type for EBS-based storage.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoVolumeType :: Lens.Lens' EBSOptions (Lude.Maybe VolumeType)
eoVolumeType = Lens.lens (volumeType :: EBSOptions -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: EBSOptions)
{-# DEPRECATED eoVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | Specifies whether EBS-based storage is enabled.
--
-- /Note:/ Consider using 'ebsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoEBSEnabled :: Lens.Lens' EBSOptions (Lude.Maybe Lude.Bool)
eoEBSEnabled = Lens.lens (ebsEnabled :: EBSOptions -> Lude.Maybe Lude.Bool) (\s a -> s {ebsEnabled = a} :: EBSOptions)
{-# DEPRECATED eoEBSEnabled "Use generic-lens or generic-optics with 'ebsEnabled' instead." #-}

instance Lude.FromJSON EBSOptions where
  parseJSON =
    Lude.withObject
      "EBSOptions"
      ( \x ->
          EBSOptions'
            Lude.<$> (x Lude..:? "VolumeSize")
            Lude.<*> (x Lude..:? "Iops")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "EBSEnabled")
      )

instance Lude.ToJSON EBSOptions where
  toJSON EBSOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VolumeSize" Lude..=) Lude.<$> volumeSize,
            ("Iops" Lude..=) Lude.<$> iops,
            ("VolumeType" Lude..=) Lude.<$> volumeType,
            ("EBSEnabled" Lude..=) Lude.<$> ebsEnabled
          ]
      )

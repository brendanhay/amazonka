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
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options to enable, disable, and specify the properties of EBS storage
-- volumes. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>.
--
-- /See:/ 'newEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Integer to specify the size of an EBS volume.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
    iops :: Prelude.Maybe Prelude.Int,
    -- | Specifies the volume type for EBS-based storage.
    volumeType :: Prelude.Maybe VolumeType,
    -- | Specifies whether EBS-based storage is enabled.
    eBSEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeSize', 'eBSOptions_volumeSize' - Integer to specify the size of an EBS volume.
--
-- 'iops', 'eBSOptions_iops' - Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- 'volumeType', 'eBSOptions_volumeType' - Specifies the volume type for EBS-based storage.
--
-- 'eBSEnabled', 'eBSOptions_eBSEnabled' - Specifies whether EBS-based storage is enabled.
newEBSOptions ::
  EBSOptions
newEBSOptions =
  EBSOptions'
    { volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      eBSEnabled = Prelude.Nothing
    }

-- | Integer to specify the size of an EBS volume.
eBSOptions_volumeSize :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_volumeSize = Lens.lens (\EBSOptions' {volumeSize} -> volumeSize) (\s@EBSOptions' {} a -> s {volumeSize = a} :: EBSOptions)

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
eBSOptions_iops :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_iops = Lens.lens (\EBSOptions' {iops} -> iops) (\s@EBSOptions' {} a -> s {iops = a} :: EBSOptions)

-- | Specifies the volume type for EBS-based storage.
eBSOptions_volumeType :: Lens.Lens' EBSOptions (Prelude.Maybe VolumeType)
eBSOptions_volumeType = Lens.lens (\EBSOptions' {volumeType} -> volumeType) (\s@EBSOptions' {} a -> s {volumeType = a} :: EBSOptions)

-- | Specifies whether EBS-based storage is enabled.
eBSOptions_eBSEnabled :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Bool)
eBSOptions_eBSEnabled = Lens.lens (\EBSOptions' {eBSEnabled} -> eBSEnabled) (\s@EBSOptions' {} a -> s {eBSEnabled = a} :: EBSOptions)

instance Core.FromJSON EBSOptions where
  parseJSON =
    Core.withObject
      "EBSOptions"
      ( \x ->
          EBSOptions'
            Prelude.<$> (x Core..:? "VolumeSize")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "VolumeType")
            Prelude.<*> (x Core..:? "EBSEnabled")
      )

instance Prelude.Hashable EBSOptions

instance Prelude.NFData EBSOptions

instance Core.ToJSON EBSOptions where
  toJSON EBSOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VolumeSize" Core..=) Prelude.<$> volumeSize,
            ("Iops" Core..=) Prelude.<$> iops,
            ("VolumeType" Core..=) Prelude.<$> volumeType,
            ("EBSEnabled" Core..=) Prelude.<$> eBSEnabled
          ]
      )

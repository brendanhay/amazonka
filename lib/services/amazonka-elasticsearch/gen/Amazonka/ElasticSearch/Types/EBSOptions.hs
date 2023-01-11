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
-- Module      : Amazonka.ElasticSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.EBSOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Options to enable, disable, and specify the properties of EBS storage
-- volumes. For more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>.
--
-- /See:/ 'newEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Specifies whether EBS-based storage is enabled.
    eBSEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the IOPS for Provisioned IOPS And GP3 EBS volume (SSD).
    iops :: Prelude.Maybe Prelude.Int,
    -- | Specifies the Throughput for GP3 EBS volume (SSD).
    throughput :: Prelude.Maybe Prelude.Int,
    -- | Integer to specify the size of an EBS volume.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies the volume type for EBS-based storage.
    volumeType :: Prelude.Maybe VolumeType
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
-- 'eBSEnabled', 'eBSOptions_eBSEnabled' - Specifies whether EBS-based storage is enabled.
--
-- 'iops', 'eBSOptions_iops' - Specifies the IOPS for Provisioned IOPS And GP3 EBS volume (SSD).
--
-- 'throughput', 'eBSOptions_throughput' - Specifies the Throughput for GP3 EBS volume (SSD).
--
-- 'volumeSize', 'eBSOptions_volumeSize' - Integer to specify the size of an EBS volume.
--
-- 'volumeType', 'eBSOptions_volumeType' - Specifies the volume type for EBS-based storage.
newEBSOptions ::
  EBSOptions
newEBSOptions =
  EBSOptions'
    { eBSEnabled = Prelude.Nothing,
      iops = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Specifies whether EBS-based storage is enabled.
eBSOptions_eBSEnabled :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Bool)
eBSOptions_eBSEnabled = Lens.lens (\EBSOptions' {eBSEnabled} -> eBSEnabled) (\s@EBSOptions' {} a -> s {eBSEnabled = a} :: EBSOptions)

-- | Specifies the IOPS for Provisioned IOPS And GP3 EBS volume (SSD).
eBSOptions_iops :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_iops = Lens.lens (\EBSOptions' {iops} -> iops) (\s@EBSOptions' {} a -> s {iops = a} :: EBSOptions)

-- | Specifies the Throughput for GP3 EBS volume (SSD).
eBSOptions_throughput :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_throughput = Lens.lens (\EBSOptions' {throughput} -> throughput) (\s@EBSOptions' {} a -> s {throughput = a} :: EBSOptions)

-- | Integer to specify the size of an EBS volume.
eBSOptions_volumeSize :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_volumeSize = Lens.lens (\EBSOptions' {volumeSize} -> volumeSize) (\s@EBSOptions' {} a -> s {volumeSize = a} :: EBSOptions)

-- | Specifies the volume type for EBS-based storage.
eBSOptions_volumeType :: Lens.Lens' EBSOptions (Prelude.Maybe VolumeType)
eBSOptions_volumeType = Lens.lens (\EBSOptions' {volumeType} -> volumeType) (\s@EBSOptions' {} a -> s {volumeType = a} :: EBSOptions)

instance Data.FromJSON EBSOptions where
  parseJSON =
    Data.withObject
      "EBSOptions"
      ( \x ->
          EBSOptions'
            Prelude.<$> (x Data..:? "EBSEnabled")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "Throughput")
            Prelude.<*> (x Data..:? "VolumeSize")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable EBSOptions where
  hashWithSalt _salt EBSOptions' {..} =
    _salt `Prelude.hashWithSalt` eBSEnabled
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData EBSOptions where
  rnf EBSOptions' {..} =
    Prelude.rnf eBSEnabled
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf volumeType

instance Data.ToJSON EBSOptions where
  toJSON EBSOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EBSEnabled" Data..=) Prelude.<$> eBSEnabled,
            ("Iops" Data..=) Prelude.<$> iops,
            ("Throughput" Data..=) Prelude.<$> throughput,
            ("VolumeSize" Data..=) Prelude.<$> volumeSize,
            ("VolumeType" Data..=) Prelude.<$> volumeType
          ]
      )

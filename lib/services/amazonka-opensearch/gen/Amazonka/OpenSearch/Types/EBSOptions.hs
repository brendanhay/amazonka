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
-- Module      : Amazonka.OpenSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.EBSOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Container for the parameters required to enable EBS-based storage for an
-- OpenSearch Service domain.
--
-- /See:/ 'newEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Indicates whether EBS volumes are attached to data nodes in an
    -- OpenSearch Service domain.
    eBSEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the baseline input\/output (I\/O) performance of EBS volumes
    -- attached to data nodes. Applicable only for the @gp3@ and provisioned
    -- IOPS EBS volume types.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Specifies the throughput (in MiB\/s) of the EBS volumes attached to data
    -- nodes. Applicable only for the @gp3@ volume type.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | Specifies the size (in GiB) of EBS volumes attached to data nodes.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies the type of EBS volumes attached to data nodes.
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
-- 'eBSEnabled', 'eBSOptions_eBSEnabled' - Indicates whether EBS volumes are attached to data nodes in an
-- OpenSearch Service domain.
--
-- 'iops', 'eBSOptions_iops' - Specifies the baseline input\/output (I\/O) performance of EBS volumes
-- attached to data nodes. Applicable only for the @gp3@ and provisioned
-- IOPS EBS volume types.
--
-- 'throughput', 'eBSOptions_throughput' - Specifies the throughput (in MiB\/s) of the EBS volumes attached to data
-- nodes. Applicable only for the @gp3@ volume type.
--
-- 'volumeSize', 'eBSOptions_volumeSize' - Specifies the size (in GiB) of EBS volumes attached to data nodes.
--
-- 'volumeType', 'eBSOptions_volumeType' - Specifies the type of EBS volumes attached to data nodes.
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

-- | Indicates whether EBS volumes are attached to data nodes in an
-- OpenSearch Service domain.
eBSOptions_eBSEnabled :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Bool)
eBSOptions_eBSEnabled = Lens.lens (\EBSOptions' {eBSEnabled} -> eBSEnabled) (\s@EBSOptions' {} a -> s {eBSEnabled = a} :: EBSOptions)

-- | Specifies the baseline input\/output (I\/O) performance of EBS volumes
-- attached to data nodes. Applicable only for the @gp3@ and provisioned
-- IOPS EBS volume types.
eBSOptions_iops :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_iops = Lens.lens (\EBSOptions' {iops} -> iops) (\s@EBSOptions' {} a -> s {iops = a} :: EBSOptions)

-- | Specifies the throughput (in MiB\/s) of the EBS volumes attached to data
-- nodes. Applicable only for the @gp3@ volume type.
eBSOptions_throughput :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_throughput = Lens.lens (\EBSOptions' {throughput} -> throughput) (\s@EBSOptions' {} a -> s {throughput = a} :: EBSOptions)

-- | Specifies the size (in GiB) of EBS volumes attached to data nodes.
eBSOptions_volumeSize :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_volumeSize = Lens.lens (\EBSOptions' {volumeSize} -> volumeSize) (\s@EBSOptions' {} a -> s {volumeSize = a} :: EBSOptions)

-- | Specifies the type of EBS volumes attached to data nodes.
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
    _salt
      `Prelude.hashWithSalt` eBSEnabled
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData EBSOptions where
  rnf EBSOptions' {..} =
    Prelude.rnf eBSEnabled `Prelude.seq`
      Prelude.rnf iops `Prelude.seq`
        Prelude.rnf throughput `Prelude.seq`
          Prelude.rnf volumeSize `Prelude.seq`
            Prelude.rnf volumeType

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

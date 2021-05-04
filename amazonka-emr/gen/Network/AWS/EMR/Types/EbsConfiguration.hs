{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.EbsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EbsConfiguration where

import Network.AWS.EMR.Types.EbsBlockDeviceConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon EBS configuration of a cluster instance.
--
-- /See:/ 'newEbsConfiguration' smart constructor.
data EbsConfiguration = EbsConfiguration'
  { -- | An array of Amazon EBS volume specifications attached to a cluster
    -- instance.
    ebsBlockDeviceConfigs :: Prelude.Maybe [EbsBlockDeviceConfig],
    -- | Indicates whether an Amazon EBS volume is EBS-optimized.
    ebsOptimized :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EbsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsBlockDeviceConfigs', 'ebsConfiguration_ebsBlockDeviceConfigs' - An array of Amazon EBS volume specifications attached to a cluster
-- instance.
--
-- 'ebsOptimized', 'ebsConfiguration_ebsOptimized' - Indicates whether an Amazon EBS volume is EBS-optimized.
newEbsConfiguration ::
  EbsConfiguration
newEbsConfiguration =
  EbsConfiguration'
    { ebsBlockDeviceConfigs =
        Prelude.Nothing,
      ebsOptimized = Prelude.Nothing
    }

-- | An array of Amazon EBS volume specifications attached to a cluster
-- instance.
ebsConfiguration_ebsBlockDeviceConfigs :: Lens.Lens' EbsConfiguration (Prelude.Maybe [EbsBlockDeviceConfig])
ebsConfiguration_ebsBlockDeviceConfigs = Lens.lens (\EbsConfiguration' {ebsBlockDeviceConfigs} -> ebsBlockDeviceConfigs) (\s@EbsConfiguration' {} a -> s {ebsBlockDeviceConfigs = a} :: EbsConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether an Amazon EBS volume is EBS-optimized.
ebsConfiguration_ebsOptimized :: Lens.Lens' EbsConfiguration (Prelude.Maybe Prelude.Bool)
ebsConfiguration_ebsOptimized = Lens.lens (\EbsConfiguration' {ebsOptimized} -> ebsOptimized) (\s@EbsConfiguration' {} a -> s {ebsOptimized = a} :: EbsConfiguration)

instance Prelude.Hashable EbsConfiguration

instance Prelude.NFData EbsConfiguration

instance Prelude.ToJSON EbsConfiguration where
  toJSON EbsConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EbsBlockDeviceConfigs" Prelude..=)
              Prelude.<$> ebsBlockDeviceConfigs,
            ("EbsOptimized" Prelude..=)
              Prelude.<$> ebsOptimized
          ]
      )

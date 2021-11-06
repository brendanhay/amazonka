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
-- Module      : Amazonka.ImageBuilder.Types.InstanceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.InstanceConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines a custom base AMI and block device mapping configurations of an
-- instance used for building and testing container images.
--
-- /See:/ 'newInstanceConfiguration' smart constructor.
data InstanceConfiguration = InstanceConfiguration'
  { -- | The AMI ID to use as the base image for a container build and test
    -- instance. If not specified, Image Builder will use the appropriate
    -- ECS-optimized AMI as a base image.
    image :: Prelude.Maybe Prelude.Text,
    -- | Defines the block devices to attach for building an instance from this
    -- Image Builder AMI.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'instanceConfiguration_image' - The AMI ID to use as the base image for a container build and test
-- instance. If not specified, Image Builder will use the appropriate
-- ECS-optimized AMI as a base image.
--
-- 'blockDeviceMappings', 'instanceConfiguration_blockDeviceMappings' - Defines the block devices to attach for building an instance from this
-- Image Builder AMI.
newInstanceConfiguration ::
  InstanceConfiguration
newInstanceConfiguration =
  InstanceConfiguration'
    { image = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing
    }

-- | The AMI ID to use as the base image for a container build and test
-- instance. If not specified, Image Builder will use the appropriate
-- ECS-optimized AMI as a base image.
instanceConfiguration_image :: Lens.Lens' InstanceConfiguration (Prelude.Maybe Prelude.Text)
instanceConfiguration_image = Lens.lens (\InstanceConfiguration' {image} -> image) (\s@InstanceConfiguration' {} a -> s {image = a} :: InstanceConfiguration)

-- | Defines the block devices to attach for building an instance from this
-- Image Builder AMI.
instanceConfiguration_blockDeviceMappings :: Lens.Lens' InstanceConfiguration (Prelude.Maybe [InstanceBlockDeviceMapping])
instanceConfiguration_blockDeviceMappings = Lens.lens (\InstanceConfiguration' {blockDeviceMappings} -> blockDeviceMappings) (\s@InstanceConfiguration' {} a -> s {blockDeviceMappings = a} :: InstanceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON InstanceConfiguration where
  parseJSON =
    Core.withObject
      "InstanceConfiguration"
      ( \x ->
          InstanceConfiguration'
            Prelude.<$> (x Core..:? "image")
            Prelude.<*> ( x Core..:? "blockDeviceMappings"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceConfiguration

instance Prelude.NFData InstanceConfiguration

instance Core.ToJSON InstanceConfiguration where
  toJSON InstanceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("image" Core..=) Prelude.<$> image,
            ("blockDeviceMappings" Core..=)
              Prelude.<$> blockDeviceMappings
          ]
      )

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.InstanceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping
import qualified Amazonka.Prelude as Prelude

-- | Defines a custom base AMI and block device mapping configurations of an
-- instance used for building and testing container images.
--
-- /See:/ 'newInstanceConfiguration' smart constructor.
data InstanceConfiguration = InstanceConfiguration'
  { -- | Defines the block devices to attach for building an instance from this
    -- Image Builder AMI.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The AMI ID to use as the base image for a container build and test
    -- instance. If not specified, Image Builder will use the appropriate
    -- ECS-optimized AMI as a base image.
    image :: Prelude.Maybe Prelude.Text
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
-- 'blockDeviceMappings', 'instanceConfiguration_blockDeviceMappings' - Defines the block devices to attach for building an instance from this
-- Image Builder AMI.
--
-- 'image', 'instanceConfiguration_image' - The AMI ID to use as the base image for a container build and test
-- instance. If not specified, Image Builder will use the appropriate
-- ECS-optimized AMI as a base image.
newInstanceConfiguration ::
  InstanceConfiguration
newInstanceConfiguration =
  InstanceConfiguration'
    { blockDeviceMappings =
        Prelude.Nothing,
      image = Prelude.Nothing
    }

-- | Defines the block devices to attach for building an instance from this
-- Image Builder AMI.
instanceConfiguration_blockDeviceMappings :: Lens.Lens' InstanceConfiguration (Prelude.Maybe [InstanceBlockDeviceMapping])
instanceConfiguration_blockDeviceMappings = Lens.lens (\InstanceConfiguration' {blockDeviceMappings} -> blockDeviceMappings) (\s@InstanceConfiguration' {} a -> s {blockDeviceMappings = a} :: InstanceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The AMI ID to use as the base image for a container build and test
-- instance. If not specified, Image Builder will use the appropriate
-- ECS-optimized AMI as a base image.
instanceConfiguration_image :: Lens.Lens' InstanceConfiguration (Prelude.Maybe Prelude.Text)
instanceConfiguration_image = Lens.lens (\InstanceConfiguration' {image} -> image) (\s@InstanceConfiguration' {} a -> s {image = a} :: InstanceConfiguration)

instance Data.FromJSON InstanceConfiguration where
  parseJSON =
    Data.withObject
      "InstanceConfiguration"
      ( \x ->
          InstanceConfiguration'
            Prelude.<$> ( x
                            Data..:? "blockDeviceMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "image")
      )

instance Prelude.Hashable InstanceConfiguration where
  hashWithSalt _salt InstanceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` image

instance Prelude.NFData InstanceConfiguration where
  rnf InstanceConfiguration' {..} =
    Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf image

instance Data.ToJSON InstanceConfiguration where
  toJSON InstanceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("blockDeviceMappings" Data..=)
              Prelude.<$> blockDeviceMappings,
            ("image" Data..=) Prelude.<$> image
          ]
      )

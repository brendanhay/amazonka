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
-- Module      : Network.AWS.ECS.Types.HostVolumeProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostVolumeProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on a container instance bind mount host volume.
--
-- /See:/ 'newHostVolumeProperties' smart constructor.
data HostVolumeProperties = HostVolumeProperties'
  { -- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
    -- path on the host container instance that is presented to the container.
    -- If this parameter is empty, then the Docker daemon has assigned a host
    -- path for you. If the @host@ parameter contains a @sourcePath@ file
    -- location, then the data volume persists at the specified location on the
    -- host container instance until you delete it manually. If the
    -- @sourcePath@ value does not exist on the host container instance, the
    -- Docker daemon creates it. If the location does exist, the contents of
    -- the source path folder are exported.
    --
    -- If you are using the Fargate launch type, the @sourcePath@ parameter is
    -- not supported.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HostVolumeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'hostVolumeProperties_sourcePath' - When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that is presented to the container.
-- If this parameter is empty, then the Docker daemon has assigned a host
-- path for you. If the @host@ parameter contains a @sourcePath@ file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the
-- @sourcePath@ value does not exist on the host container instance, the
-- Docker daemon creates it. If the location does exist, the contents of
-- the source path folder are exported.
--
-- If you are using the Fargate launch type, the @sourcePath@ parameter is
-- not supported.
newHostVolumeProperties ::
  HostVolumeProperties
newHostVolumeProperties =
  HostVolumeProperties' {sourcePath = Prelude.Nothing}

-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that is presented to the container.
-- If this parameter is empty, then the Docker daemon has assigned a host
-- path for you. If the @host@ parameter contains a @sourcePath@ file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the
-- @sourcePath@ value does not exist on the host container instance, the
-- Docker daemon creates it. If the location does exist, the contents of
-- the source path folder are exported.
--
-- If you are using the Fargate launch type, the @sourcePath@ parameter is
-- not supported.
hostVolumeProperties_sourcePath :: Lens.Lens' HostVolumeProperties (Prelude.Maybe Prelude.Text)
hostVolumeProperties_sourcePath = Lens.lens (\HostVolumeProperties' {sourcePath} -> sourcePath) (\s@HostVolumeProperties' {} a -> s {sourcePath = a} :: HostVolumeProperties)

instance Prelude.FromJSON HostVolumeProperties where
  parseJSON =
    Prelude.withObject
      "HostVolumeProperties"
      ( \x ->
          HostVolumeProperties'
            Prelude.<$> (x Prelude..:? "sourcePath")
      )

instance Prelude.Hashable HostVolumeProperties

instance Prelude.NFData HostVolumeProperties

instance Prelude.ToJSON HostVolumeProperties where
  toJSON HostVolumeProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("sourcePath" Prelude..=) Prelude.<$> sourcePath]
      )

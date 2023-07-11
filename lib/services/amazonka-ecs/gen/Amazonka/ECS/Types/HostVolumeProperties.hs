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
-- Module      : Amazonka.ECS.Types.HostVolumeProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.HostVolumeProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on a container instance bind mount host volume.
--
-- /See:/ 'newHostVolumeProperties' smart constructor.
data HostVolumeProperties = HostVolumeProperties'
  { -- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
    -- path on the host container instance that\'s presented to the container.
    -- If this parameter is empty, then the Docker daemon has assigned a host
    -- path for you. If the @host@ parameter contains a @sourcePath@ file
    -- location, then the data volume persists at the specified location on the
    -- host container instance until you delete it manually. If the
    -- @sourcePath@ value doesn\'t exist on the host container instance, the
    -- Docker daemon creates it. If the location does exist, the contents of
    -- the source path folder are exported.
    --
    -- If you\'re using the Fargate launch type, the @sourcePath@ parameter is
    -- not supported.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostVolumeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'hostVolumeProperties_sourcePath' - When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that\'s presented to the container.
-- If this parameter is empty, then the Docker daemon has assigned a host
-- path for you. If the @host@ parameter contains a @sourcePath@ file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the
-- @sourcePath@ value doesn\'t exist on the host container instance, the
-- Docker daemon creates it. If the location does exist, the contents of
-- the source path folder are exported.
--
-- If you\'re using the Fargate launch type, the @sourcePath@ parameter is
-- not supported.
newHostVolumeProperties ::
  HostVolumeProperties
newHostVolumeProperties =
  HostVolumeProperties' {sourcePath = Prelude.Nothing}

-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that\'s presented to the container.
-- If this parameter is empty, then the Docker daemon has assigned a host
-- path for you. If the @host@ parameter contains a @sourcePath@ file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the
-- @sourcePath@ value doesn\'t exist on the host container instance, the
-- Docker daemon creates it. If the location does exist, the contents of
-- the source path folder are exported.
--
-- If you\'re using the Fargate launch type, the @sourcePath@ parameter is
-- not supported.
hostVolumeProperties_sourcePath :: Lens.Lens' HostVolumeProperties (Prelude.Maybe Prelude.Text)
hostVolumeProperties_sourcePath = Lens.lens (\HostVolumeProperties' {sourcePath} -> sourcePath) (\s@HostVolumeProperties' {} a -> s {sourcePath = a} :: HostVolumeProperties)

instance Data.FromJSON HostVolumeProperties where
  parseJSON =
    Data.withObject
      "HostVolumeProperties"
      ( \x ->
          HostVolumeProperties'
            Prelude.<$> (x Data..:? "sourcePath")
      )

instance Prelude.Hashable HostVolumeProperties where
  hashWithSalt _salt HostVolumeProperties' {..} =
    _salt `Prelude.hashWithSalt` sourcePath

instance Prelude.NFData HostVolumeProperties where
  rnf HostVolumeProperties' {..} =
    Prelude.rnf sourcePath

instance Data.ToJSON HostVolumeProperties where
  toJSON HostVolumeProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sourcePath" Data..=) Prelude.<$> sourcePath]
      )

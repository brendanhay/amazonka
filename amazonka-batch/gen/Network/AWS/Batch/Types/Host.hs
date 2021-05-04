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
-- Module      : Network.AWS.Batch.Types.Host
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Host where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Determine whether your data volume persists on the host container
-- instance and where it is stored. If this parameter is empty, then the
-- Docker daemon assigns a host path for your data volume, but the data
-- isn\'t guaranteed to persist after the containers associated with it
-- stop running.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | The path on the host container instance that\'s presented to the
    -- container. If this parameter is empty, then the Docker daemon has
    -- assigned a host path for you. If this parameter contains a file
    -- location, then the data volume persists at the specified location on the
    -- host container instance until you delete it manually. If the source path
    -- location does not exist on the host container instance, the Docker
    -- daemon creates it. If the location does exist, the contents of the
    -- source path folder are exported.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Host' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'host_sourcePath' - The path on the host container instance that\'s presented to the
-- container. If this parameter is empty, then the Docker daemon has
-- assigned a host path for you. If this parameter contains a file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the source path
-- location does not exist on the host container instance, the Docker
-- daemon creates it. If the location does exist, the contents of the
-- source path folder are exported.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
newHost ::
  Host
newHost = Host' {sourcePath = Prelude.Nothing}

-- | The path on the host container instance that\'s presented to the
-- container. If this parameter is empty, then the Docker daemon has
-- assigned a host path for you. If this parameter contains a file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the source path
-- location does not exist on the host container instance, the Docker
-- daemon creates it. If the location does exist, the contents of the
-- source path folder are exported.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
host_sourcePath :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_sourcePath = Lens.lens (\Host' {sourcePath} -> sourcePath) (\s@Host' {} a -> s {sourcePath = a} :: Host)

instance Prelude.FromJSON Host where
  parseJSON =
    Prelude.withObject
      "Host"
      ( \x ->
          Host' Prelude.<$> (x Prelude..:? "sourcePath")
      )

instance Prelude.Hashable Host

instance Prelude.NFData Host

instance Prelude.ToJSON Host where
  toJSON Host' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("sourcePath" Prelude..=) Prelude.<$> sourcePath]
      )

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
-- Module      : Amazonka.Batch.Types.Host
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.Host where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determine whether your data volume persists on the host container
-- instance and where it\'s stored. If this parameter is empty, then the
-- Docker daemon assigns a host path for your data volume. However, the
-- data isn\'t guaranteed to persist after the containers that are
-- associated with it stop running.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | The path on the host container instance that\'s presented to the
    -- container. If this parameter is empty, then the Docker daemon has
    -- assigned a host path for you. If this parameter contains a file
    -- location, then the data volume persists at the specified location on the
    -- host container instance until you delete it manually. If the source path
    -- location doesn\'t exist on the host container instance, the Docker
    -- daemon creates it. If the location does exist, the contents of the
    -- source path folder are exported.
    --
    -- This parameter isn\'t applicable to jobs that run on Fargate resources.
    -- Don\'t provide this for these jobs.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- location doesn\'t exist on the host container instance, the Docker
-- daemon creates it. If the location does exist, the contents of the
-- source path folder are exported.
--
-- This parameter isn\'t applicable to jobs that run on Fargate resources.
-- Don\'t provide this for these jobs.
newHost ::
  Host
newHost = Host' {sourcePath = Prelude.Nothing}

-- | The path on the host container instance that\'s presented to the
-- container. If this parameter is empty, then the Docker daemon has
-- assigned a host path for you. If this parameter contains a file
-- location, then the data volume persists at the specified location on the
-- host container instance until you delete it manually. If the source path
-- location doesn\'t exist on the host container instance, the Docker
-- daemon creates it. If the location does exist, the contents of the
-- source path folder are exported.
--
-- This parameter isn\'t applicable to jobs that run on Fargate resources.
-- Don\'t provide this for these jobs.
host_sourcePath :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_sourcePath = Lens.lens (\Host' {sourcePath} -> sourcePath) (\s@Host' {} a -> s {sourcePath = a} :: Host)

instance Data.FromJSON Host where
  parseJSON =
    Data.withObject
      "Host"
      (\x -> Host' Prelude.<$> (x Data..:? "sourcePath"))

instance Prelude.Hashable Host where
  hashWithSalt _salt Host' {..} =
    _salt `Prelude.hashWithSalt` sourcePath

instance Prelude.NFData Host where
  rnf Host' {..} = Prelude.rnf sourcePath

instance Data.ToJSON Host where
  toJSON Host' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sourcePath" Data..=) Prelude.<$> sourcePath]
      )

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
-- Module      : Network.AWS.SageMaker.Types.ProcessingResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingResources where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingClusterConfig

-- | Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a processing job. In distributed training, you specify
-- more than one instance.
--
-- /See:/ 'newProcessingResources' smart constructor.
data ProcessingResources = ProcessingResources'
  { -- | The configuration for the resources in a cluster used to run the
    -- processing job.
    clusterConfig :: ProcessingClusterConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessingResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterConfig', 'processingResources_clusterConfig' - The configuration for the resources in a cluster used to run the
-- processing job.
newProcessingResources ::
  -- | 'clusterConfig'
  ProcessingClusterConfig ->
  ProcessingResources
newProcessingResources pClusterConfig_ =
  ProcessingResources'
    { clusterConfig =
        pClusterConfig_
    }

-- | The configuration for the resources in a cluster used to run the
-- processing job.
processingResources_clusterConfig :: Lens.Lens' ProcessingResources ProcessingClusterConfig
processingResources_clusterConfig = Lens.lens (\ProcessingResources' {clusterConfig} -> clusterConfig) (\s@ProcessingResources' {} a -> s {clusterConfig = a} :: ProcessingResources)

instance Prelude.FromJSON ProcessingResources where
  parseJSON =
    Prelude.withObject
      "ProcessingResources"
      ( \x ->
          ProcessingResources'
            Prelude.<$> (x Prelude..: "ClusterConfig")
      )

instance Prelude.Hashable ProcessingResources

instance Prelude.NFData ProcessingResources

instance Prelude.ToJSON ProcessingResources where
  toJSON ProcessingResources' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClusterConfig" Prelude..= clusterConfig)
          ]
      )

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
-- Module      : Network.AWS.OpsWorks.Types.EcsCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EcsCluster where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a registered Amazon ECS cluster.
--
-- /See:/ 'newEcsCluster' smart constructor.
data EcsCluster = EcsCluster'
  { -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The cluster name.
    ecsClusterName :: Prelude.Maybe Prelude.Text,
    -- | The time and date that the cluster was registered with the stack.
    registeredAt :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s ARN.
    ecsClusterArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EcsCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'ecsCluster_stackId' - The stack ID.
--
-- 'ecsClusterName', 'ecsCluster_ecsClusterName' - The cluster name.
--
-- 'registeredAt', 'ecsCluster_registeredAt' - The time and date that the cluster was registered with the stack.
--
-- 'ecsClusterArn', 'ecsCluster_ecsClusterArn' - The cluster\'s ARN.
newEcsCluster ::
  EcsCluster
newEcsCluster =
  EcsCluster'
    { stackId = Prelude.Nothing,
      ecsClusterName = Prelude.Nothing,
      registeredAt = Prelude.Nothing,
      ecsClusterArn = Prelude.Nothing
    }

-- | The stack ID.
ecsCluster_stackId :: Lens.Lens' EcsCluster (Prelude.Maybe Prelude.Text)
ecsCluster_stackId = Lens.lens (\EcsCluster' {stackId} -> stackId) (\s@EcsCluster' {} a -> s {stackId = a} :: EcsCluster)

-- | The cluster name.
ecsCluster_ecsClusterName :: Lens.Lens' EcsCluster (Prelude.Maybe Prelude.Text)
ecsCluster_ecsClusterName = Lens.lens (\EcsCluster' {ecsClusterName} -> ecsClusterName) (\s@EcsCluster' {} a -> s {ecsClusterName = a} :: EcsCluster)

-- | The time and date that the cluster was registered with the stack.
ecsCluster_registeredAt :: Lens.Lens' EcsCluster (Prelude.Maybe Prelude.Text)
ecsCluster_registeredAt = Lens.lens (\EcsCluster' {registeredAt} -> registeredAt) (\s@EcsCluster' {} a -> s {registeredAt = a} :: EcsCluster)

-- | The cluster\'s ARN.
ecsCluster_ecsClusterArn :: Lens.Lens' EcsCluster (Prelude.Maybe Prelude.Text)
ecsCluster_ecsClusterArn = Lens.lens (\EcsCluster' {ecsClusterArn} -> ecsClusterArn) (\s@EcsCluster' {} a -> s {ecsClusterArn = a} :: EcsCluster)

instance Prelude.FromJSON EcsCluster where
  parseJSON =
    Prelude.withObject
      "EcsCluster"
      ( \x ->
          EcsCluster'
            Prelude.<$> (x Prelude..:? "StackId")
            Prelude.<*> (x Prelude..:? "EcsClusterName")
            Prelude.<*> (x Prelude..:? "RegisteredAt")
            Prelude.<*> (x Prelude..:? "EcsClusterArn")
      )

instance Prelude.Hashable EcsCluster

instance Prelude.NFData EcsCluster

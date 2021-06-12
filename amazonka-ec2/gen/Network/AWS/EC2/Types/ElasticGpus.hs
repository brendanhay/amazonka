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
-- Module      : Network.AWS.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpus where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an Elastic Graphics accelerator.
--
-- /See:/ 'newElasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { -- | The type of Elastic Graphics accelerator.
    elasticGpuType :: Core.Maybe Core.Text,
    -- | The ID of the instance to which the Elastic Graphics accelerator is
    -- attached.
    instanceId :: Core.Maybe Core.Text,
    -- | The status of the Elastic Graphics accelerator.
    elasticGpuHealth :: Core.Maybe ElasticGpuHealth,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Core.Maybe Core.Text,
    -- | The Availability Zone in the which the Elastic Graphics accelerator
    -- resides.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The tags assigned to the Elastic Graphics accelerator.
    tags :: Core.Maybe [Tag],
    -- | The state of the Elastic Graphics accelerator.
    elasticGpuState :: Core.Maybe ElasticGpuState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticGpus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticGpuType', 'elasticGpus_elasticGpuType' - The type of Elastic Graphics accelerator.
--
-- 'instanceId', 'elasticGpus_instanceId' - The ID of the instance to which the Elastic Graphics accelerator is
-- attached.
--
-- 'elasticGpuHealth', 'elasticGpus_elasticGpuHealth' - The status of the Elastic Graphics accelerator.
--
-- 'elasticGpuId', 'elasticGpus_elasticGpuId' - The ID of the Elastic Graphics accelerator.
--
-- 'availabilityZone', 'elasticGpus_availabilityZone' - The Availability Zone in the which the Elastic Graphics accelerator
-- resides.
--
-- 'tags', 'elasticGpus_tags' - The tags assigned to the Elastic Graphics accelerator.
--
-- 'elasticGpuState', 'elasticGpus_elasticGpuState' - The state of the Elastic Graphics accelerator.
newElasticGpus ::
  ElasticGpus
newElasticGpus =
  ElasticGpus'
    { elasticGpuType = Core.Nothing,
      instanceId = Core.Nothing,
      elasticGpuHealth = Core.Nothing,
      elasticGpuId = Core.Nothing,
      availabilityZone = Core.Nothing,
      tags = Core.Nothing,
      elasticGpuState = Core.Nothing
    }

-- | The type of Elastic Graphics accelerator.
elasticGpus_elasticGpuType :: Lens.Lens' ElasticGpus (Core.Maybe Core.Text)
elasticGpus_elasticGpuType = Lens.lens (\ElasticGpus' {elasticGpuType} -> elasticGpuType) (\s@ElasticGpus' {} a -> s {elasticGpuType = a} :: ElasticGpus)

-- | The ID of the instance to which the Elastic Graphics accelerator is
-- attached.
elasticGpus_instanceId :: Lens.Lens' ElasticGpus (Core.Maybe Core.Text)
elasticGpus_instanceId = Lens.lens (\ElasticGpus' {instanceId} -> instanceId) (\s@ElasticGpus' {} a -> s {instanceId = a} :: ElasticGpus)

-- | The status of the Elastic Graphics accelerator.
elasticGpus_elasticGpuHealth :: Lens.Lens' ElasticGpus (Core.Maybe ElasticGpuHealth)
elasticGpus_elasticGpuHealth = Lens.lens (\ElasticGpus' {elasticGpuHealth} -> elasticGpuHealth) (\s@ElasticGpus' {} a -> s {elasticGpuHealth = a} :: ElasticGpus)

-- | The ID of the Elastic Graphics accelerator.
elasticGpus_elasticGpuId :: Lens.Lens' ElasticGpus (Core.Maybe Core.Text)
elasticGpus_elasticGpuId = Lens.lens (\ElasticGpus' {elasticGpuId} -> elasticGpuId) (\s@ElasticGpus' {} a -> s {elasticGpuId = a} :: ElasticGpus)

-- | The Availability Zone in the which the Elastic Graphics accelerator
-- resides.
elasticGpus_availabilityZone :: Lens.Lens' ElasticGpus (Core.Maybe Core.Text)
elasticGpus_availabilityZone = Lens.lens (\ElasticGpus' {availabilityZone} -> availabilityZone) (\s@ElasticGpus' {} a -> s {availabilityZone = a} :: ElasticGpus)

-- | The tags assigned to the Elastic Graphics accelerator.
elasticGpus_tags :: Lens.Lens' ElasticGpus (Core.Maybe [Tag])
elasticGpus_tags = Lens.lens (\ElasticGpus' {tags} -> tags) (\s@ElasticGpus' {} a -> s {tags = a} :: ElasticGpus) Core.. Lens.mapping Lens._Coerce

-- | The state of the Elastic Graphics accelerator.
elasticGpus_elasticGpuState :: Lens.Lens' ElasticGpus (Core.Maybe ElasticGpuState)
elasticGpus_elasticGpuState = Lens.lens (\ElasticGpus' {elasticGpuState} -> elasticGpuState) (\s@ElasticGpus' {} a -> s {elasticGpuState = a} :: ElasticGpus)

instance Core.FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      Core.<$> (x Core..@? "elasticGpuType")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "elasticGpuHealth")
      Core.<*> (x Core..@? "elasticGpuId")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "elasticGpuState")

instance Core.Hashable ElasticGpus

instance Core.NFData ElasticGpus

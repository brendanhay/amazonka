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
-- Module      : Network.AWS.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Elastic Graphics accelerator.
--
-- /See:/ 'newElasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { -- | The type of Elastic Graphics accelerator.
    elasticGpuType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance to which the Elastic Graphics accelerator is
    -- attached.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The status of the Elastic Graphics accelerator.
    elasticGpuHealth :: Prelude.Maybe ElasticGpuHealth,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in the which the Elastic Graphics accelerator
    -- resides.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the Elastic Graphics accelerator.
    tags :: Prelude.Maybe [Tag],
    -- | The state of the Elastic Graphics accelerator.
    elasticGpuState :: Prelude.Maybe ElasticGpuState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { elasticGpuType = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      elasticGpuHealth = Prelude.Nothing,
      elasticGpuId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      tags = Prelude.Nothing,
      elasticGpuState = Prelude.Nothing
    }

-- | The type of Elastic Graphics accelerator.
elasticGpus_elasticGpuType :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_elasticGpuType = Lens.lens (\ElasticGpus' {elasticGpuType} -> elasticGpuType) (\s@ElasticGpus' {} a -> s {elasticGpuType = a} :: ElasticGpus)

-- | The ID of the instance to which the Elastic Graphics accelerator is
-- attached.
elasticGpus_instanceId :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_instanceId = Lens.lens (\ElasticGpus' {instanceId} -> instanceId) (\s@ElasticGpus' {} a -> s {instanceId = a} :: ElasticGpus)

-- | The status of the Elastic Graphics accelerator.
elasticGpus_elasticGpuHealth :: Lens.Lens' ElasticGpus (Prelude.Maybe ElasticGpuHealth)
elasticGpus_elasticGpuHealth = Lens.lens (\ElasticGpus' {elasticGpuHealth} -> elasticGpuHealth) (\s@ElasticGpus' {} a -> s {elasticGpuHealth = a} :: ElasticGpus)

-- | The ID of the Elastic Graphics accelerator.
elasticGpus_elasticGpuId :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_elasticGpuId = Lens.lens (\ElasticGpus' {elasticGpuId} -> elasticGpuId) (\s@ElasticGpus' {} a -> s {elasticGpuId = a} :: ElasticGpus)

-- | The Availability Zone in the which the Elastic Graphics accelerator
-- resides.
elasticGpus_availabilityZone :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_availabilityZone = Lens.lens (\ElasticGpus' {availabilityZone} -> availabilityZone) (\s@ElasticGpus' {} a -> s {availabilityZone = a} :: ElasticGpus)

-- | The tags assigned to the Elastic Graphics accelerator.
elasticGpus_tags :: Lens.Lens' ElasticGpus (Prelude.Maybe [Tag])
elasticGpus_tags = Lens.lens (\ElasticGpus' {tags} -> tags) (\s@ElasticGpus' {} a -> s {tags = a} :: ElasticGpus) Prelude.. Lens.mapping Prelude._Coerce

-- | The state of the Elastic Graphics accelerator.
elasticGpus_elasticGpuState :: Lens.Lens' ElasticGpus (Prelude.Maybe ElasticGpuState)
elasticGpus_elasticGpuState = Lens.lens (\ElasticGpus' {elasticGpuState} -> elasticGpuState) (\s@ElasticGpus' {} a -> s {elasticGpuState = a} :: ElasticGpus)

instance Prelude.FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      Prelude.<$> (x Prelude..@? "elasticGpuType")
      Prelude.<*> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "elasticGpuHealth")
      Prelude.<*> (x Prelude..@? "elasticGpuId")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "elasticGpuState")

instance Prelude.Hashable ElasticGpus

instance Prelude.NFData ElasticGpus

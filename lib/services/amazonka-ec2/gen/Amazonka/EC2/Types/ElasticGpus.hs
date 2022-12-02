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
-- Module      : Amazonka.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ElasticGpus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ElasticGpuHealth
import Amazonka.EC2.Types.ElasticGpuState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Elastic Graphics accelerator.
--
-- /See:/ 'newElasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { -- | The tags assigned to the Elastic Graphics accelerator.
    tags :: Prelude.Maybe [Tag],
    -- | The status of the Elastic Graphics accelerator.
    elasticGpuHealth :: Prelude.Maybe ElasticGpuHealth,
    -- | The Availability Zone in the which the Elastic Graphics accelerator
    -- resides.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The state of the Elastic Graphics accelerator.
    elasticGpuState :: Prelude.Maybe ElasticGpuState,
    -- | The ID of the instance to which the Elastic Graphics accelerator is
    -- attached.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Prelude.Maybe Prelude.Text,
    -- | The type of Elastic Graphics accelerator.
    elasticGpuType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticGpus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'elasticGpus_tags' - The tags assigned to the Elastic Graphics accelerator.
--
-- 'elasticGpuHealth', 'elasticGpus_elasticGpuHealth' - The status of the Elastic Graphics accelerator.
--
-- 'availabilityZone', 'elasticGpus_availabilityZone' - The Availability Zone in the which the Elastic Graphics accelerator
-- resides.
--
-- 'elasticGpuState', 'elasticGpus_elasticGpuState' - The state of the Elastic Graphics accelerator.
--
-- 'instanceId', 'elasticGpus_instanceId' - The ID of the instance to which the Elastic Graphics accelerator is
-- attached.
--
-- 'elasticGpuId', 'elasticGpus_elasticGpuId' - The ID of the Elastic Graphics accelerator.
--
-- 'elasticGpuType', 'elasticGpus_elasticGpuType' - The type of Elastic Graphics accelerator.
newElasticGpus ::
  ElasticGpus
newElasticGpus =
  ElasticGpus'
    { tags = Prelude.Nothing,
      elasticGpuHealth = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      elasticGpuState = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      elasticGpuId = Prelude.Nothing,
      elasticGpuType = Prelude.Nothing
    }

-- | The tags assigned to the Elastic Graphics accelerator.
elasticGpus_tags :: Lens.Lens' ElasticGpus (Prelude.Maybe [Tag])
elasticGpus_tags = Lens.lens (\ElasticGpus' {tags} -> tags) (\s@ElasticGpus' {} a -> s {tags = a} :: ElasticGpus) Prelude.. Lens.mapping Lens.coerced

-- | The status of the Elastic Graphics accelerator.
elasticGpus_elasticGpuHealth :: Lens.Lens' ElasticGpus (Prelude.Maybe ElasticGpuHealth)
elasticGpus_elasticGpuHealth = Lens.lens (\ElasticGpus' {elasticGpuHealth} -> elasticGpuHealth) (\s@ElasticGpus' {} a -> s {elasticGpuHealth = a} :: ElasticGpus)

-- | The Availability Zone in the which the Elastic Graphics accelerator
-- resides.
elasticGpus_availabilityZone :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_availabilityZone = Lens.lens (\ElasticGpus' {availabilityZone} -> availabilityZone) (\s@ElasticGpus' {} a -> s {availabilityZone = a} :: ElasticGpus)

-- | The state of the Elastic Graphics accelerator.
elasticGpus_elasticGpuState :: Lens.Lens' ElasticGpus (Prelude.Maybe ElasticGpuState)
elasticGpus_elasticGpuState = Lens.lens (\ElasticGpus' {elasticGpuState} -> elasticGpuState) (\s@ElasticGpus' {} a -> s {elasticGpuState = a} :: ElasticGpus)

-- | The ID of the instance to which the Elastic Graphics accelerator is
-- attached.
elasticGpus_instanceId :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_instanceId = Lens.lens (\ElasticGpus' {instanceId} -> instanceId) (\s@ElasticGpus' {} a -> s {instanceId = a} :: ElasticGpus)

-- | The ID of the Elastic Graphics accelerator.
elasticGpus_elasticGpuId :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_elasticGpuId = Lens.lens (\ElasticGpus' {elasticGpuId} -> elasticGpuId) (\s@ElasticGpus' {} a -> s {elasticGpuId = a} :: ElasticGpus)

-- | The type of Elastic Graphics accelerator.
elasticGpus_elasticGpuType :: Lens.Lens' ElasticGpus (Prelude.Maybe Prelude.Text)
elasticGpus_elasticGpuType = Lens.lens (\ElasticGpus' {elasticGpuType} -> elasticGpuType) (\s@ElasticGpus' {} a -> s {elasticGpuType = a} :: ElasticGpus)

instance Data.FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "elasticGpuHealth")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "elasticGpuState")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "elasticGpuId")
      Prelude.<*> (x Data..@? "elasticGpuType")

instance Prelude.Hashable ElasticGpus where
  hashWithSalt _salt ElasticGpus' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` elasticGpuHealth
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` elasticGpuState
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` elasticGpuId
      `Prelude.hashWithSalt` elasticGpuType

instance Prelude.NFData ElasticGpus where
  rnf ElasticGpus' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf elasticGpuHealth
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf elasticGpuState
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf elasticGpuId
      `Prelude.seq` Prelude.rnf elasticGpuType

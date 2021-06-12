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
-- Module      : Network.AWS.EC2.Types.ElasticGpuAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the association between an instance and an Elastic Graphics
-- accelerator.
--
-- /See:/ 'newElasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { -- | The time the Elastic Graphics accelerator was associated with the
    -- instance.
    elasticGpuAssociationTime :: Core.Maybe Core.Text,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Core.Maybe Core.Text,
    -- | The state of the association between the instance and the Elastic
    -- Graphics accelerator.
    elasticGpuAssociationState :: Core.Maybe Core.Text,
    -- | The ID of the association.
    elasticGpuAssociationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticGpuAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticGpuAssociationTime', 'elasticGpuAssociation_elasticGpuAssociationTime' - The time the Elastic Graphics accelerator was associated with the
-- instance.
--
-- 'elasticGpuId', 'elasticGpuAssociation_elasticGpuId' - The ID of the Elastic Graphics accelerator.
--
-- 'elasticGpuAssociationState', 'elasticGpuAssociation_elasticGpuAssociationState' - The state of the association between the instance and the Elastic
-- Graphics accelerator.
--
-- 'elasticGpuAssociationId', 'elasticGpuAssociation_elasticGpuAssociationId' - The ID of the association.
newElasticGpuAssociation ::
  ElasticGpuAssociation
newElasticGpuAssociation =
  ElasticGpuAssociation'
    { elasticGpuAssociationTime =
        Core.Nothing,
      elasticGpuId = Core.Nothing,
      elasticGpuAssociationState = Core.Nothing,
      elasticGpuAssociationId = Core.Nothing
    }

-- | The time the Elastic Graphics accelerator was associated with the
-- instance.
elasticGpuAssociation_elasticGpuAssociationTime :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
elasticGpuAssociation_elasticGpuAssociationTime = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationTime} -> elasticGpuAssociationTime) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationTime = a} :: ElasticGpuAssociation)

-- | The ID of the Elastic Graphics accelerator.
elasticGpuAssociation_elasticGpuId :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
elasticGpuAssociation_elasticGpuId = Lens.lens (\ElasticGpuAssociation' {elasticGpuId} -> elasticGpuId) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuId = a} :: ElasticGpuAssociation)

-- | The state of the association between the instance and the Elastic
-- Graphics accelerator.
elasticGpuAssociation_elasticGpuAssociationState :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
elasticGpuAssociation_elasticGpuAssociationState = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationState} -> elasticGpuAssociationState) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationState = a} :: ElasticGpuAssociation)

-- | The ID of the association.
elasticGpuAssociation_elasticGpuAssociationId :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
elasticGpuAssociation_elasticGpuAssociationId = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationId} -> elasticGpuAssociationId) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationId = a} :: ElasticGpuAssociation)

instance Core.FromXML ElasticGpuAssociation where
  parseXML x =
    ElasticGpuAssociation'
      Core.<$> (x Core..@? "elasticGpuAssociationTime")
      Core.<*> (x Core..@? "elasticGpuId")
      Core.<*> (x Core..@? "elasticGpuAssociationState")
      Core.<*> (x Core..@? "elasticGpuAssociationId")

instance Core.Hashable ElasticGpuAssociation

instance Core.NFData ElasticGpuAssociation

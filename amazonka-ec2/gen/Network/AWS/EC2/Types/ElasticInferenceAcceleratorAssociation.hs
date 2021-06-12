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
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the association between an instance and an elastic inference
-- accelerator.
--
-- /See:/ 'newElasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { -- | The ID of the association.
    elasticInferenceAcceleratorAssociationId :: Core.Maybe Core.Text,
    -- | The state of the elastic inference accelerator.
    elasticInferenceAcceleratorAssociationState :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
    elasticInferenceAcceleratorArn :: Core.Maybe Core.Text,
    -- | The time at which the elastic inference accelerator is associated with
    -- an instance.
    elasticInferenceAcceleratorAssociationTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticInferenceAcceleratorAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticInferenceAcceleratorAssociationId', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId' - The ID of the association.
--
-- 'elasticInferenceAcceleratorAssociationState', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState' - The state of the elastic inference accelerator.
--
-- 'elasticInferenceAcceleratorArn', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn' - The Amazon Resource Name (ARN) of the elastic inference accelerator.
--
-- 'elasticInferenceAcceleratorAssociationTime', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime' - The time at which the elastic inference accelerator is associated with
-- an instance.
newElasticInferenceAcceleratorAssociation ::
  ElasticInferenceAcceleratorAssociation
newElasticInferenceAcceleratorAssociation =
  ElasticInferenceAcceleratorAssociation'
    { elasticInferenceAcceleratorAssociationId =
        Core.Nothing,
      elasticInferenceAcceleratorAssociationState =
        Core.Nothing,
      elasticInferenceAcceleratorArn =
        Core.Nothing,
      elasticInferenceAcceleratorAssociationTime =
        Core.Nothing
    }

-- | The ID of the association.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationId} -> elasticInferenceAcceleratorAssociationId) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationId = a} :: ElasticInferenceAcceleratorAssociation)

-- | The state of the elastic inference accelerator.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationState} -> elasticInferenceAcceleratorAssociationState) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationState = a} :: ElasticInferenceAcceleratorAssociation)

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorArn} -> elasticInferenceAcceleratorArn) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorArn = a} :: ElasticInferenceAcceleratorAssociation)

-- | The time at which the elastic inference accelerator is associated with
-- an instance.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.UTCTime)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationTime} -> elasticInferenceAcceleratorAssociationTime) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationTime = a} :: ElasticInferenceAcceleratorAssociation) Core.. Lens.mapping Core._Time

instance
  Core.FromXML
    ElasticInferenceAcceleratorAssociation
  where
  parseXML x =
    ElasticInferenceAcceleratorAssociation'
      Core.<$> ( x
                   Core..@? "elasticInferenceAcceleratorAssociationId"
               )
      Core.<*> ( x
                   Core..@? "elasticInferenceAcceleratorAssociationState"
               )
      Core.<*> (x Core..@? "elasticInferenceAcceleratorArn")
      Core.<*> ( x
                   Core..@? "elasticInferenceAcceleratorAssociationTime"
               )

instance
  Core.Hashable
    ElasticInferenceAcceleratorAssociation

instance
  Core.NFData
    ElasticInferenceAcceleratorAssociation

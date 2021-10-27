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
-- Module      : Network.AWS.ElasticInference.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Types.ElasticInferenceAccelerator where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticInference.Types.ElasticInferenceAcceleratorHealth
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of an Elastic Inference Accelerator.
--
-- /See:/ 'newElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { -- | The type of the Elastic Inference Accelerator.
    acceleratorType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Elastic Inference Accelerator.
    acceleratorId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource that the Elastic Inference Accelerator is
    -- attached to.
    attachedResource :: Prelude.Maybe Prelude.Text,
    -- | The health of the Elastic Inference Accelerator.
    acceleratorHealth :: Prelude.Maybe ElasticInferenceAcceleratorHealth,
    -- | The availability zone where the Elastic Inference Accelerator is
    -- present.
    availabilityZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticInferenceAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorType', 'elasticInferenceAccelerator_acceleratorType' - The type of the Elastic Inference Accelerator.
--
-- 'acceleratorId', 'elasticInferenceAccelerator_acceleratorId' - The ID of the Elastic Inference Accelerator.
--
-- 'attachedResource', 'elasticInferenceAccelerator_attachedResource' - The ARN of the resource that the Elastic Inference Accelerator is
-- attached to.
--
-- 'acceleratorHealth', 'elasticInferenceAccelerator_acceleratorHealth' - The health of the Elastic Inference Accelerator.
--
-- 'availabilityZone', 'elasticInferenceAccelerator_availabilityZone' - The availability zone where the Elastic Inference Accelerator is
-- present.
newElasticInferenceAccelerator ::
  ElasticInferenceAccelerator
newElasticInferenceAccelerator =
  ElasticInferenceAccelerator'
    { acceleratorType =
        Prelude.Nothing,
      acceleratorId = Prelude.Nothing,
      attachedResource = Prelude.Nothing,
      acceleratorHealth = Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The type of the Elastic Inference Accelerator.
elasticInferenceAccelerator_acceleratorType :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe Prelude.Text)
elasticInferenceAccelerator_acceleratorType = Lens.lens (\ElasticInferenceAccelerator' {acceleratorType} -> acceleratorType) (\s@ElasticInferenceAccelerator' {} a -> s {acceleratorType = a} :: ElasticInferenceAccelerator)

-- | The ID of the Elastic Inference Accelerator.
elasticInferenceAccelerator_acceleratorId :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe Prelude.Text)
elasticInferenceAccelerator_acceleratorId = Lens.lens (\ElasticInferenceAccelerator' {acceleratorId} -> acceleratorId) (\s@ElasticInferenceAccelerator' {} a -> s {acceleratorId = a} :: ElasticInferenceAccelerator)

-- | The ARN of the resource that the Elastic Inference Accelerator is
-- attached to.
elasticInferenceAccelerator_attachedResource :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe Prelude.Text)
elasticInferenceAccelerator_attachedResource = Lens.lens (\ElasticInferenceAccelerator' {attachedResource} -> attachedResource) (\s@ElasticInferenceAccelerator' {} a -> s {attachedResource = a} :: ElasticInferenceAccelerator)

-- | The health of the Elastic Inference Accelerator.
elasticInferenceAccelerator_acceleratorHealth :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe ElasticInferenceAcceleratorHealth)
elasticInferenceAccelerator_acceleratorHealth = Lens.lens (\ElasticInferenceAccelerator' {acceleratorHealth} -> acceleratorHealth) (\s@ElasticInferenceAccelerator' {} a -> s {acceleratorHealth = a} :: ElasticInferenceAccelerator)

-- | The availability zone where the Elastic Inference Accelerator is
-- present.
elasticInferenceAccelerator_availabilityZone :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe Prelude.Text)
elasticInferenceAccelerator_availabilityZone = Lens.lens (\ElasticInferenceAccelerator' {availabilityZone} -> availabilityZone) (\s@ElasticInferenceAccelerator' {} a -> s {availabilityZone = a} :: ElasticInferenceAccelerator)

instance Core.FromJSON ElasticInferenceAccelerator where
  parseJSON =
    Core.withObject
      "ElasticInferenceAccelerator"
      ( \x ->
          ElasticInferenceAccelerator'
            Prelude.<$> (x Core..:? "acceleratorType")
            Prelude.<*> (x Core..:? "acceleratorId")
            Prelude.<*> (x Core..:? "attachedResource")
            Prelude.<*> (x Core..:? "acceleratorHealth")
            Prelude.<*> (x Core..:? "availabilityZone")
      )

instance Prelude.Hashable ElasticInferenceAccelerator

instance Prelude.NFData ElasticInferenceAccelerator

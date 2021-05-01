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
-- Module      : Network.AWS.ECS.Types.ResourceRequirement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ResourceRequirement where

import Network.AWS.ECS.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type and amount of a resource to assign to a container. The
-- supported resource types are GPUs and Elastic Inference accelerators.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS>
-- or
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/
--
-- /See:/ 'newResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { -- | The value for the specified resource type.
    --
    -- If the @GPU@ type is used, the value is the number of physical @GPUs@
    -- the Amazon ECS container agent will reserve for the container. The
    -- number of GPUs reserved for all containers in a task should not exceed
    -- the number of available GPUs on the container instance the task is
    -- launched on.
    --
    -- If the @InferenceAccelerator@ type is used, the @value@ should match the
    -- @deviceName@ for an InferenceAccelerator specified in a task definition.
    value :: Prelude.Text,
    -- | The type of resource to assign to a container. The supported values are
    -- @GPU@ or @InferenceAccelerator@.
    type' :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceRequirement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceRequirement_value' - The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@
-- the Amazon ECS container agent will reserve for the container. The
-- number of GPUs reserved for all containers in a task should not exceed
-- the number of available GPUs on the container instance the task is
-- launched on.
--
-- If the @InferenceAccelerator@ type is used, the @value@ should match the
-- @deviceName@ for an InferenceAccelerator specified in a task definition.
--
-- 'type'', 'resourceRequirement_type' - The type of resource to assign to a container. The supported values are
-- @GPU@ or @InferenceAccelerator@.
newResourceRequirement ::
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  ResourceType ->
  ResourceRequirement
newResourceRequirement pValue_ pType_ =
  ResourceRequirement'
    { value = pValue_,
      type' = pType_
    }

-- | The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@
-- the Amazon ECS container agent will reserve for the container. The
-- number of GPUs reserved for all containers in a task should not exceed
-- the number of available GPUs on the container instance the task is
-- launched on.
--
-- If the @InferenceAccelerator@ type is used, the @value@ should match the
-- @deviceName@ for an InferenceAccelerator specified in a task definition.
resourceRequirement_value :: Lens.Lens' ResourceRequirement Prelude.Text
resourceRequirement_value = Lens.lens (\ResourceRequirement' {value} -> value) (\s@ResourceRequirement' {} a -> s {value = a} :: ResourceRequirement)

-- | The type of resource to assign to a container. The supported values are
-- @GPU@ or @InferenceAccelerator@.
resourceRequirement_type :: Lens.Lens' ResourceRequirement ResourceType
resourceRequirement_type = Lens.lens (\ResourceRequirement' {type'} -> type') (\s@ResourceRequirement' {} a -> s {type' = a} :: ResourceRequirement)

instance Prelude.FromJSON ResourceRequirement where
  parseJSON =
    Prelude.withObject
      "ResourceRequirement"
      ( \x ->
          ResourceRequirement'
            Prelude.<$> (x Prelude..: "value")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable ResourceRequirement

instance Prelude.NFData ResourceRequirement

instance Prelude.ToJSON ResourceRequirement where
  toJSON ResourceRequirement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Prelude..= value),
            Prelude.Just ("type" Prelude..= type')
          ]
      )

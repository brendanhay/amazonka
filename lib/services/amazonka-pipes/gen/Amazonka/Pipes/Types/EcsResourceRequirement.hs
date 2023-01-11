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
-- Module      : Amazonka.Pipes.Types.EcsResourceRequirement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsResourceRequirement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.EcsResourceRequirementType
import qualified Amazonka.Prelude as Prelude

-- | The type and amount of a resource to assign to a container. The
-- supported resource types are GPUs and Elastic Inference accelerators.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS>
-- or
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-inference.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/
--
-- /See:/ 'newEcsResourceRequirement' smart constructor.
data EcsResourceRequirement = EcsResourceRequirement'
  { -- | The type of resource to assign to a container. The supported values are
    -- @GPU@ or @InferenceAccelerator@.
    type' :: EcsResourceRequirementType,
    -- | The value for the specified resource type.
    --
    -- If the @GPU@ type is used, the value is the number of physical @GPUs@
    -- the Amazon ECS container agent reserves for the container. The number of
    -- GPUs that\'s reserved for all containers in a task can\'t exceed the
    -- number of available GPUs on the container instance that the task is
    -- launched on.
    --
    -- If the @InferenceAccelerator@ type is used, the @value@ matches the
    -- @deviceName@ for an InferenceAccelerator specified in a task definition.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsResourceRequirement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'ecsResourceRequirement_type' - The type of resource to assign to a container. The supported values are
-- @GPU@ or @InferenceAccelerator@.
--
-- 'value', 'ecsResourceRequirement_value' - The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@
-- the Amazon ECS container agent reserves for the container. The number of
-- GPUs that\'s reserved for all containers in a task can\'t exceed the
-- number of available GPUs on the container instance that the task is
-- launched on.
--
-- If the @InferenceAccelerator@ type is used, the @value@ matches the
-- @deviceName@ for an InferenceAccelerator specified in a task definition.
newEcsResourceRequirement ::
  -- | 'type''
  EcsResourceRequirementType ->
  -- | 'value'
  Prelude.Text ->
  EcsResourceRequirement
newEcsResourceRequirement pType_ pValue_ =
  EcsResourceRequirement'
    { type' = pType_,
      value = pValue_
    }

-- | The type of resource to assign to a container. The supported values are
-- @GPU@ or @InferenceAccelerator@.
ecsResourceRequirement_type :: Lens.Lens' EcsResourceRequirement EcsResourceRequirementType
ecsResourceRequirement_type = Lens.lens (\EcsResourceRequirement' {type'} -> type') (\s@EcsResourceRequirement' {} a -> s {type' = a} :: EcsResourceRequirement)

-- | The value for the specified resource type.
--
-- If the @GPU@ type is used, the value is the number of physical @GPUs@
-- the Amazon ECS container agent reserves for the container. The number of
-- GPUs that\'s reserved for all containers in a task can\'t exceed the
-- number of available GPUs on the container instance that the task is
-- launched on.
--
-- If the @InferenceAccelerator@ type is used, the @value@ matches the
-- @deviceName@ for an InferenceAccelerator specified in a task definition.
ecsResourceRequirement_value :: Lens.Lens' EcsResourceRequirement Prelude.Text
ecsResourceRequirement_value = Lens.lens (\EcsResourceRequirement' {value} -> value) (\s@EcsResourceRequirement' {} a -> s {value = a} :: EcsResourceRequirement)

instance Data.FromJSON EcsResourceRequirement where
  parseJSON =
    Data.withObject
      "EcsResourceRequirement"
      ( \x ->
          EcsResourceRequirement'
            Prelude.<$> (x Data..: "type") Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable EcsResourceRequirement where
  hashWithSalt _salt EcsResourceRequirement' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData EcsResourceRequirement where
  rnf EcsResourceRequirement' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EcsResourceRequirement where
  toJSON EcsResourceRequirement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("value" Data..= value)
          ]
      )

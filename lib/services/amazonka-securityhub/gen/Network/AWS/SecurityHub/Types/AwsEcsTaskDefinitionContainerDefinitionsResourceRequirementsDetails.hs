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
-- Module      : Network.AWS.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A resource to assign to a container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails = AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
  { -- | The value for the specified resource type.
    --
    -- For @GPU@, the value is the number of physical GPUs the Amazon ECS
    -- container agent reserves for the container.
    --
    -- For @InferenceAccelerator@, the value should match the @DeviceName@
    -- attribute of an entry in @InferenceAccelerators@.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of resource to assign to a container.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value' - The value for the specified resource type.
--
-- For @GPU@, the value is the number of physical GPUs the Amazon ECS
-- container agent reserves for the container.
--
-- For @InferenceAccelerator@, the value should match the @DeviceName@
-- attribute of an entry in @InferenceAccelerators@.
--
-- 'type'', 'awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type' - The type of resource to assign to a container.
newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
    { value =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | The value for the specified resource type.
--
-- For @GPU@, the value is the number of physical GPUs the Amazon ECS
-- container agent reserves for the container.
--
-- For @InferenceAccelerator@, the value should match the @DeviceName@
-- attribute of an entry in @InferenceAccelerators@.
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {value} -> value) (\s@AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails)

-- | The type of resource to assign to a container.
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Value" Core..=) Prelude.<$> value,
              ("Type" Core..=) Prelude.<$> type'
            ]
        )

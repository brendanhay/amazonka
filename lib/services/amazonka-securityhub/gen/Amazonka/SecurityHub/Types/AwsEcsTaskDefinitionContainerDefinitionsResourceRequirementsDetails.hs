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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource to assign to a container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails = AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
  { -- | The type of resource to assign to a container. Valid values are @GPU@ or
    -- @InferenceAccelerator@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The value for the specified resource type.
    --
    -- For @GPU@, the value is the number of physical GPUs the Amazon ECS
    -- container agent reserves for the container.
    --
    -- For @InferenceAccelerator@, the value should match the @DeviceName@
    -- attribute of an entry in @InferenceAccelerators@.
    value :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type' - The type of resource to assign to a container. Valid values are @GPU@ or
-- @InferenceAccelerator@.
--
-- 'value', 'awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value' - The value for the specified resource type.
--
-- For @GPU@, the value is the number of physical GPUs the Amazon ECS
-- container agent reserves for the container.
--
-- For @InferenceAccelerator@, the value should match the @DeviceName@
-- attribute of an entry in @InferenceAccelerators@.
newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
    { type' =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The type of resource to assign to a container. Valid values are @GPU@ or
-- @InferenceAccelerator@.
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails)

-- | The value for the specified resource type.
--
-- For @GPU@, the value is the number of physical GPUs the Amazon ECS
-- container agent reserves for the container.
--
-- For @InferenceAccelerator@, the value should match the @DeviceName@
-- attribute of an entry in @InferenceAccelerators@.
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {value} -> value) (\s@AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {..} =
      Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Type" Data..=) Prelude.<$> type',
              ("Value" Data..=) Prelude.<$> value
            ]
        )

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
-- Module      : Amazonka.SageMaker.Types.VariantProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.VariantProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VariantPropertyType

-- | Specifies a production variant property type for an Endpoint.
--
-- If you are updating an endpoint with the @RetainAllVariantProperties@
-- option of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpoint.html UpdateEndpointInput>
-- set to @true@, the @VariantProperty@ objects listed in the
-- @ExcludeRetainedVariantProperties@ parameter of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpoint.html UpdateEndpointInput>
-- override the existing variant properties of the endpoint.
--
-- /See:/ 'newVariantProperty' smart constructor.
data VariantProperty = VariantProperty'
  { -- | The type of variant property. The supported values are:
    --
    -- -   @DesiredInstanceCount@: Overrides the existing variant instance
    --     counts using the @InitialInstanceCount@ values in the
    --     @ProductionVariants@ of
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
    --
    -- -   @DesiredWeight@: Overrides the existing variant weights using the
    --     @InitialVariantWeight@ values in the @ProductionVariants@ of
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
    --
    -- -   @DataCaptureConfig@: (Not currently supported.)
    variantPropertyType :: VariantPropertyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariantProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variantPropertyType', 'variantProperty_variantPropertyType' - The type of variant property. The supported values are:
--
-- -   @DesiredInstanceCount@: Overrides the existing variant instance
--     counts using the @InitialInstanceCount@ values in the
--     @ProductionVariants@ of
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
--
-- -   @DesiredWeight@: Overrides the existing variant weights using the
--     @InitialVariantWeight@ values in the @ProductionVariants@ of
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
--
-- -   @DataCaptureConfig@: (Not currently supported.)
newVariantProperty ::
  -- | 'variantPropertyType'
  VariantPropertyType ->
  VariantProperty
newVariantProperty pVariantPropertyType_ =
  VariantProperty'
    { variantPropertyType =
        pVariantPropertyType_
    }

-- | The type of variant property. The supported values are:
--
-- -   @DesiredInstanceCount@: Overrides the existing variant instance
--     counts using the @InitialInstanceCount@ values in the
--     @ProductionVariants@ of
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
--
-- -   @DesiredWeight@: Overrides the existing variant weights using the
--     @InitialVariantWeight@ values in the @ProductionVariants@ of
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>.
--
-- -   @DataCaptureConfig@: (Not currently supported.)
variantProperty_variantPropertyType :: Lens.Lens' VariantProperty VariantPropertyType
variantProperty_variantPropertyType = Lens.lens (\VariantProperty' {variantPropertyType} -> variantPropertyType) (\s@VariantProperty' {} a -> s {variantPropertyType = a} :: VariantProperty)

instance Prelude.Hashable VariantProperty where
  hashWithSalt _salt VariantProperty' {..} =
    _salt `Prelude.hashWithSalt` variantPropertyType

instance Prelude.NFData VariantProperty where
  rnf VariantProperty' {..} =
    Prelude.rnf variantPropertyType

instance Data.ToJSON VariantProperty where
  toJSON VariantProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VariantPropertyType" Data..= variantPropertyType)
          ]
      )

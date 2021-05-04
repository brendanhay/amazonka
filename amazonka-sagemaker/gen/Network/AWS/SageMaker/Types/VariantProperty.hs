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
-- Module      : Network.AWS.SageMaker.Types.VariantProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.VariantPropertyType

-- | Specifies a production variant property type for an Endpoint.
--
-- If you are updating an endpoint with the
-- UpdateEndpointInput$RetainAllVariantProperties option set to @true@, the
-- @VariantProperty@ objects listed in
-- UpdateEndpointInput$ExcludeRetainedVariantProperties override the
-- existing variant properties of the endpoint.
--
-- /See:/ 'newVariantProperty' smart constructor.
data VariantProperty = VariantProperty'
  { -- | The type of variant property. The supported values are:
    --
    -- -   @DesiredInstanceCount@: Overrides the existing variant instance
    --     counts using the ProductionVariant$InitialInstanceCount values in
    --     the CreateEndpointConfigInput$ProductionVariants.
    --
    -- -   @DesiredWeight@: Overrides the existing variant weights using the
    --     ProductionVariant$InitialVariantWeight values in the
    --     CreateEndpointConfigInput$ProductionVariants.
    --
    -- -   @DataCaptureConfig@: (Not currently supported.)
    variantPropertyType :: VariantPropertyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
--     counts using the ProductionVariant$InitialInstanceCount values in
--     the CreateEndpointConfigInput$ProductionVariants.
--
-- -   @DesiredWeight@: Overrides the existing variant weights using the
--     ProductionVariant$InitialVariantWeight values in the
--     CreateEndpointConfigInput$ProductionVariants.
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
--     counts using the ProductionVariant$InitialInstanceCount values in
--     the CreateEndpointConfigInput$ProductionVariants.
--
-- -   @DesiredWeight@: Overrides the existing variant weights using the
--     ProductionVariant$InitialVariantWeight values in the
--     CreateEndpointConfigInput$ProductionVariants.
--
-- -   @DataCaptureConfig@: (Not currently supported.)
variantProperty_variantPropertyType :: Lens.Lens' VariantProperty VariantPropertyType
variantProperty_variantPropertyType = Lens.lens (\VariantProperty' {variantPropertyType} -> variantPropertyType) (\s@VariantProperty' {} a -> s {variantPropertyType = a} :: VariantProperty)

instance Prelude.Hashable VariantProperty

instance Prelude.NFData VariantProperty

instance Prelude.ToJSON VariantProperty where
  toJSON VariantProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VariantPropertyType"
                  Prelude..= variantPropertyType
              )
          ]
      )

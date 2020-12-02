{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantProperty where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.VariantPropertyType

-- | Specifies a production variant property type for an Endpoint.
--
--
-- If you are updating an endpoint with the 'UpdateEndpointInput$RetainAllVariantProperties' option set to @true@ , the @VariantProperty@ objects listed in 'UpdateEndpointInput$ExcludeRetainedVariantProperties' override the existing variant properties of the endpoint.
--
--
-- /See:/ 'variantProperty' smart constructor.
newtype VariantProperty = VariantProperty'
  { _vpVariantPropertyType ::
      VariantPropertyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VariantProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpVariantPropertyType' - The type of variant property. The supported values are:     * @DesiredInstanceCount@ : Overrides the existing variant instance counts using the 'ProductionVariant$InitialInstanceCount' values in the 'CreateEndpointConfigInput$ProductionVariants' .     * @DesiredWeight@ : Overrides the existing variant weights using the 'ProductionVariant$InitialVariantWeight' values in the 'CreateEndpointConfigInput$ProductionVariants' .     * @DataCaptureConfig@ : (Not currently supported.)
variantProperty ::
  -- | 'vpVariantPropertyType'
  VariantPropertyType ->
  VariantProperty
variantProperty pVariantPropertyType_ =
  VariantProperty' {_vpVariantPropertyType = pVariantPropertyType_}

-- | The type of variant property. The supported values are:     * @DesiredInstanceCount@ : Overrides the existing variant instance counts using the 'ProductionVariant$InitialInstanceCount' values in the 'CreateEndpointConfigInput$ProductionVariants' .     * @DesiredWeight@ : Overrides the existing variant weights using the 'ProductionVariant$InitialVariantWeight' values in the 'CreateEndpointConfigInput$ProductionVariants' .     * @DataCaptureConfig@ : (Not currently supported.)
vpVariantPropertyType :: Lens' VariantProperty VariantPropertyType
vpVariantPropertyType = lens _vpVariantPropertyType (\s a -> s {_vpVariantPropertyType = a})

instance Hashable VariantProperty

instance NFData VariantProperty

instance ToJSON VariantProperty where
  toJSON VariantProperty' {..} =
    object
      ( catMaybes
          [Just ("VariantPropertyType" .= _vpVariantPropertyType)]
      )

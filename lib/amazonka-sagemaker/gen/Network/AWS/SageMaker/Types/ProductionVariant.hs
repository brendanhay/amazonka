{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariant where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType

-- | Identifies a model that you want to host and the resources to deploy for hosting it. If you are deploying multiple models, tell Amazon SageMaker how to distribute traffic among the models by specifying variant weights.
--
--
--
-- /See:/ 'productionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { _pvAcceleratorType ::
      !(Maybe ProductionVariantAcceleratorType),
    _pvInitialVariantWeight :: !(Maybe Double),
    _pvVariantName :: !Text,
    _pvModelName :: !Text,
    _pvInitialInstanceCount :: !Nat,
    _pvInstanceType :: !ProductionVariantInstanceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductionVariant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvAcceleratorType' - The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- * 'pvInitialVariantWeight' - Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
--
-- * 'pvVariantName' - The name of the production variant.
--
-- * 'pvModelName' - The name of the model that you want to host. This is the name that you specified when creating the model.
--
-- * 'pvInitialInstanceCount' - Number of instances to launch initially.
--
-- * 'pvInstanceType' - The ML compute instance type.
productionVariant ::
  -- | 'pvVariantName'
  Text ->
  -- | 'pvModelName'
  Text ->
  -- | 'pvInitialInstanceCount'
  Natural ->
  -- | 'pvInstanceType'
  ProductionVariantInstanceType ->
  ProductionVariant
productionVariant
  pVariantName_
  pModelName_
  pInitialInstanceCount_
  pInstanceType_ =
    ProductionVariant'
      { _pvAcceleratorType = Nothing,
        _pvInitialVariantWeight = Nothing,
        _pvVariantName = pVariantName_,
        _pvModelName = pModelName_,
        _pvInitialInstanceCount = _Nat # pInitialInstanceCount_,
        _pvInstanceType = pInstanceType_
      }

-- | The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
pvAcceleratorType :: Lens' ProductionVariant (Maybe ProductionVariantAcceleratorType)
pvAcceleratorType = lens _pvAcceleratorType (\s a -> s {_pvAcceleratorType = a})

-- | Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
pvInitialVariantWeight :: Lens' ProductionVariant (Maybe Double)
pvInitialVariantWeight = lens _pvInitialVariantWeight (\s a -> s {_pvInitialVariantWeight = a})

-- | The name of the production variant.
pvVariantName :: Lens' ProductionVariant Text
pvVariantName = lens _pvVariantName (\s a -> s {_pvVariantName = a})

-- | The name of the model that you want to host. This is the name that you specified when creating the model.
pvModelName :: Lens' ProductionVariant Text
pvModelName = lens _pvModelName (\s a -> s {_pvModelName = a})

-- | Number of instances to launch initially.
pvInitialInstanceCount :: Lens' ProductionVariant Natural
pvInitialInstanceCount = lens _pvInitialInstanceCount (\s a -> s {_pvInitialInstanceCount = a}) . _Nat

-- | The ML compute instance type.
pvInstanceType :: Lens' ProductionVariant ProductionVariantInstanceType
pvInstanceType = lens _pvInstanceType (\s a -> s {_pvInstanceType = a})

instance FromJSON ProductionVariant where
  parseJSON =
    withObject
      "ProductionVariant"
      ( \x ->
          ProductionVariant'
            <$> (x .:? "AcceleratorType")
            <*> (x .:? "InitialVariantWeight")
            <*> (x .: "VariantName")
            <*> (x .: "ModelName")
            <*> (x .: "InitialInstanceCount")
            <*> (x .: "InstanceType")
      )

instance Hashable ProductionVariant

instance NFData ProductionVariant

instance ToJSON ProductionVariant where
  toJSON ProductionVariant' {..} =
    object
      ( catMaybes
          [ ("AcceleratorType" .=) <$> _pvAcceleratorType,
            ("InitialVariantWeight" .=) <$> _pvInitialVariantWeight,
            Just ("VariantName" .= _pvVariantName),
            Just ("ModelName" .= _pvModelName),
            Just ("InitialInstanceCount" .= _pvInitialInstanceCount),
            Just ("InstanceType" .= _pvInstanceType)
          ]
      )

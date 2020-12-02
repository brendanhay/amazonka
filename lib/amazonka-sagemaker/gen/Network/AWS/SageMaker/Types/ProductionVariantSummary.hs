{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.DeployedImage

-- | Describes weight and capacities for a production variant associated with an endpoint. If you sent a request to the @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is @Updating@ , you get different desired and current values.
--
--
--
-- /See:/ 'productionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { _pvsDesiredInstanceCount ::
      !(Maybe Nat),
    _pvsDesiredWeight :: !(Maybe Double),
    _pvsCurrentWeight :: !(Maybe Double),
    _pvsCurrentInstanceCount :: !(Maybe Nat),
    _pvsDeployedImages ::
      !(Maybe [DeployedImage]),
    _pvsVariantName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductionVariantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvsDesiredInstanceCount' - The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- * 'pvsDesiredWeight' - The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- * 'pvsCurrentWeight' - The weight associated with the variant.
--
-- * 'pvsCurrentInstanceCount' - The number of instances associated with the variant.
--
-- * 'pvsDeployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
--
-- * 'pvsVariantName' - The name of the variant.
productionVariantSummary ::
  -- | 'pvsVariantName'
  Text ->
  ProductionVariantSummary
productionVariantSummary pVariantName_ =
  ProductionVariantSummary'
    { _pvsDesiredInstanceCount = Nothing,
      _pvsDesiredWeight = Nothing,
      _pvsCurrentWeight = Nothing,
      _pvsCurrentInstanceCount = Nothing,
      _pvsDeployedImages = Nothing,
      _pvsVariantName = pVariantName_
    }

-- | The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
pvsDesiredInstanceCount :: Lens' ProductionVariantSummary (Maybe Natural)
pvsDesiredInstanceCount = lens _pvsDesiredInstanceCount (\s a -> s {_pvsDesiredInstanceCount = a}) . mapping _Nat

-- | The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
pvsDesiredWeight :: Lens' ProductionVariantSummary (Maybe Double)
pvsDesiredWeight = lens _pvsDesiredWeight (\s a -> s {_pvsDesiredWeight = a})

-- | The weight associated with the variant.
pvsCurrentWeight :: Lens' ProductionVariantSummary (Maybe Double)
pvsCurrentWeight = lens _pvsCurrentWeight (\s a -> s {_pvsCurrentWeight = a})

-- | The number of instances associated with the variant.
pvsCurrentInstanceCount :: Lens' ProductionVariantSummary (Maybe Natural)
pvsCurrentInstanceCount = lens _pvsCurrentInstanceCount (\s a -> s {_pvsCurrentInstanceCount = a}) . mapping _Nat

-- | An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
pvsDeployedImages :: Lens' ProductionVariantSummary [DeployedImage]
pvsDeployedImages = lens _pvsDeployedImages (\s a -> s {_pvsDeployedImages = a}) . _Default . _Coerce

-- | The name of the variant.
pvsVariantName :: Lens' ProductionVariantSummary Text
pvsVariantName = lens _pvsVariantName (\s a -> s {_pvsVariantName = a})

instance FromJSON ProductionVariantSummary where
  parseJSON =
    withObject
      "ProductionVariantSummary"
      ( \x ->
          ProductionVariantSummary'
            <$> (x .:? "DesiredInstanceCount")
            <*> (x .:? "DesiredWeight")
            <*> (x .:? "CurrentWeight")
            <*> (x .:? "CurrentInstanceCount")
            <*> (x .:? "DeployedImages" .!= mempty)
            <*> (x .: "VariantName")
      )

instance Hashable ProductionVariantSummary

instance NFData ProductionVariantSummary

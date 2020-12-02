{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Amazon ElastiCache instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'elastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { _ecidCurrentGeneration ::
      !(Maybe Bool),
    _ecidProductDescription ::
      !(Maybe Text),
    _ecidFamily :: !(Maybe Text),
    _ecidSizeFlexEligible ::
      !(Maybe Bool),
    _ecidRegion :: !(Maybe Text),
    _ecidNodeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElastiCacheInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecidCurrentGeneration' - Whether the recommendation is for a current generation instance.
--
-- * 'ecidProductDescription' - The description of the recommended reservation.
--
-- * 'ecidFamily' - The instance family of the recommended reservation.
--
-- * 'ecidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'ecidRegion' - The AWS Region of the recommended reservation.
--
-- * 'ecidNodeType' - The type of node that AWS recommends.
elastiCacheInstanceDetails ::
  ElastiCacheInstanceDetails
elastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { _ecidCurrentGeneration = Nothing,
      _ecidProductDescription = Nothing,
      _ecidFamily = Nothing,
      _ecidSizeFlexEligible = Nothing,
      _ecidRegion = Nothing,
      _ecidNodeType = Nothing
    }

-- | Whether the recommendation is for a current generation instance.
ecidCurrentGeneration :: Lens' ElastiCacheInstanceDetails (Maybe Bool)
ecidCurrentGeneration = lens _ecidCurrentGeneration (\s a -> s {_ecidCurrentGeneration = a})

-- | The description of the recommended reservation.
ecidProductDescription :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidProductDescription = lens _ecidProductDescription (\s a -> s {_ecidProductDescription = a})

-- | The instance family of the recommended reservation.
ecidFamily :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidFamily = lens _ecidFamily (\s a -> s {_ecidFamily = a})

-- | Whether the recommended reservation is size flexible.
ecidSizeFlexEligible :: Lens' ElastiCacheInstanceDetails (Maybe Bool)
ecidSizeFlexEligible = lens _ecidSizeFlexEligible (\s a -> s {_ecidSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
ecidRegion :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidRegion = lens _ecidRegion (\s a -> s {_ecidRegion = a})

-- | The type of node that AWS recommends.
ecidNodeType :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidNodeType = lens _ecidNodeType (\s a -> s {_ecidNodeType = a})

instance FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    withObject
      "ElastiCacheInstanceDetails"
      ( \x ->
          ElastiCacheInstanceDetails'
            <$> (x .:? "CurrentGeneration")
            <*> (x .:? "ProductDescription")
            <*> (x .:? "Family")
            <*> (x .:? "SizeFlexEligible")
            <*> (x .:? "Region")
            <*> (x .:? "NodeType")
      )

instance Hashable ElastiCacheInstanceDetails

instance NFData ElastiCacheInstanceDetails

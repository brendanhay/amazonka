{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ESInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ESInstanceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Amazon ES instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'eSInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { _esidCurrentGeneration ::
      !(Maybe Bool),
    _esidInstanceClass :: !(Maybe Text),
    _esidInstanceSize :: !(Maybe Text),
    _esidSizeFlexEligible :: !(Maybe Bool),
    _esidRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ESInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esidCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'esidInstanceClass' - The class of instance that AWS recommends.
--
-- * 'esidInstanceSize' - The size of instance that AWS recommends.
--
-- * 'esidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'esidRegion' - The AWS Region of the recommended reservation.
eSInstanceDetails ::
  ESInstanceDetails
eSInstanceDetails =
  ESInstanceDetails'
    { _esidCurrentGeneration = Nothing,
      _esidInstanceClass = Nothing,
      _esidInstanceSize = Nothing,
      _esidSizeFlexEligible = Nothing,
      _esidRegion = Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
esidCurrentGeneration :: Lens' ESInstanceDetails (Maybe Bool)
esidCurrentGeneration = lens _esidCurrentGeneration (\s a -> s {_esidCurrentGeneration = a})

-- | The class of instance that AWS recommends.
esidInstanceClass :: Lens' ESInstanceDetails (Maybe Text)
esidInstanceClass = lens _esidInstanceClass (\s a -> s {_esidInstanceClass = a})

-- | The size of instance that AWS recommends.
esidInstanceSize :: Lens' ESInstanceDetails (Maybe Text)
esidInstanceSize = lens _esidInstanceSize (\s a -> s {_esidInstanceSize = a})

-- | Whether the recommended reservation is size flexible.
esidSizeFlexEligible :: Lens' ESInstanceDetails (Maybe Bool)
esidSizeFlexEligible = lens _esidSizeFlexEligible (\s a -> s {_esidSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
esidRegion :: Lens' ESInstanceDetails (Maybe Text)
esidRegion = lens _esidRegion (\s a -> s {_esidRegion = a})

instance FromJSON ESInstanceDetails where
  parseJSON =
    withObject
      "ESInstanceDetails"
      ( \x ->
          ESInstanceDetails'
            <$> (x .:? "CurrentGeneration")
            <*> (x .:? "InstanceClass")
            <*> (x .:? "InstanceSize")
            <*> (x .:? "SizeFlexEligible")
            <*> (x .:? "Region")
      )

instance Hashable ESInstanceDetails

instance NFData ESInstanceDetails

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.PricingService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.PricingService where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata for a service, such as the service code and available attribute names.
--
--
--
-- /See:/ 'pricingService' smart constructor.
data PricingService = PricingService'
  { _psAttributeNames ::
      !(Maybe [Text]),
    _psServiceCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PricingService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psAttributeNames' - The attributes that are available for this service.
--
-- * 'psServiceCode' - The code for the AWS service.
pricingService ::
  PricingService
pricingService =
  PricingService'
    { _psAttributeNames = Nothing,
      _psServiceCode = Nothing
    }

-- | The attributes that are available for this service.
psAttributeNames :: Lens' PricingService [Text]
psAttributeNames = lens _psAttributeNames (\s a -> s {_psAttributeNames = a}) . _Default . _Coerce

-- | The code for the AWS service.
psServiceCode :: Lens' PricingService (Maybe Text)
psServiceCode = lens _psServiceCode (\s a -> s {_psServiceCode = a})

instance FromJSON PricingService where
  parseJSON =
    withObject
      "PricingService"
      ( \x ->
          PricingService'
            <$> (x .:? "AttributeNames" .!= mempty) <*> (x .:? "ServiceCode")
      )

instance Hashable PricingService

instance NFData PricingService

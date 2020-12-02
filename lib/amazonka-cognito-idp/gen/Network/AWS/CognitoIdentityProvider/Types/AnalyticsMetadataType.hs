{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An Amazon Pinpoint analytics endpoint.
--
--
-- An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.
--
--
-- /See:/ 'analyticsMetadataType' smart constructor.
newtype AnalyticsMetadataType = AnalyticsMetadataType'
  { _amtAnalyticsEndpointId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalyticsMetadataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amtAnalyticsEndpointId' - The endpoint ID.
analyticsMetadataType ::
  AnalyticsMetadataType
analyticsMetadataType =
  AnalyticsMetadataType' {_amtAnalyticsEndpointId = Nothing}

-- | The endpoint ID.
amtAnalyticsEndpointId :: Lens' AnalyticsMetadataType (Maybe Text)
amtAnalyticsEndpointId = lens _amtAnalyticsEndpointId (\s a -> s {_amtAnalyticsEndpointId = a})

instance Hashable AnalyticsMetadataType

instance NFData AnalyticsMetadataType

instance ToJSON AnalyticsMetadataType where
  toJSON AnalyticsMetadataType' {..} =
    object
      ( catMaybes
          [("AnalyticsEndpointId" .=) <$> _amtAnalyticsEndpointId]
      )

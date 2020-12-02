{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion where

import Network.AWS.Greengrass.Types.Subscription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a subscription definition version.
--
-- /See:/ 'subscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { _sdvSubscriptions ::
      Maybe [Subscription]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvSubscriptions' - A list of subscriptions.
subscriptionDefinitionVersion ::
  SubscriptionDefinitionVersion
subscriptionDefinitionVersion =
  SubscriptionDefinitionVersion' {_sdvSubscriptions = Nothing}

-- | A list of subscriptions.
sdvSubscriptions :: Lens' SubscriptionDefinitionVersion [Subscription]
sdvSubscriptions = lens _sdvSubscriptions (\s a -> s {_sdvSubscriptions = a}) . _Default . _Coerce

instance FromJSON SubscriptionDefinitionVersion where
  parseJSON =
    withObject
      "SubscriptionDefinitionVersion"
      ( \x ->
          SubscriptionDefinitionVersion'
            <$> (x .:? "Subscriptions" .!= mempty)
      )

instance Hashable SubscriptionDefinitionVersion

instance NFData SubscriptionDefinitionVersion

instance ToJSON SubscriptionDefinitionVersion where
  toJSON SubscriptionDefinitionVersion' {..} =
    object (catMaybes [("Subscriptions" .=) <$> _sdvSubscriptions])

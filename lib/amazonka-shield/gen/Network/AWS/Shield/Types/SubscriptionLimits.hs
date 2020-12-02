{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubscriptionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubscriptionLimits where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionLimits

-- | Limits settings for your subscription.
--
--
--
-- /See:/ 'subscriptionLimits' smart constructor.
data SubscriptionLimits = SubscriptionLimits'
  { _slProtectionLimits ::
      !ProtectionLimits,
    _slProtectionGroupLimits :: !ProtectionGroupLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscriptionLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slProtectionLimits' - Limits settings on protections for your subscription.
--
-- * 'slProtectionGroupLimits' - Limits settings on protection groups for your subscription.
subscriptionLimits ::
  -- | 'slProtectionLimits'
  ProtectionLimits ->
  -- | 'slProtectionGroupLimits'
  ProtectionGroupLimits ->
  SubscriptionLimits
subscriptionLimits pProtectionLimits_ pProtectionGroupLimits_ =
  SubscriptionLimits'
    { _slProtectionLimits = pProtectionLimits_,
      _slProtectionGroupLimits = pProtectionGroupLimits_
    }

-- | Limits settings on protections for your subscription.
slProtectionLimits :: Lens' SubscriptionLimits ProtectionLimits
slProtectionLimits = lens _slProtectionLimits (\s a -> s {_slProtectionLimits = a})

-- | Limits settings on protection groups for your subscription.
slProtectionGroupLimits :: Lens' SubscriptionLimits ProtectionGroupLimits
slProtectionGroupLimits = lens _slProtectionGroupLimits (\s a -> s {_slProtectionGroupLimits = a})

instance FromJSON SubscriptionLimits where
  parseJSON =
    withObject
      "SubscriptionLimits"
      ( \x ->
          SubscriptionLimits'
            <$> (x .: "ProtectionLimits") <*> (x .: "ProtectionGroupLimits")
      )

instance Hashable SubscriptionLimits

instance NFData SubscriptionLimits

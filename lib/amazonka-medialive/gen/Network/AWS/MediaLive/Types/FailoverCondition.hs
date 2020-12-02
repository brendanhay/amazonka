{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FailoverCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverCondition where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FailoverConditionSettings
import Network.AWS.Prelude

-- | Failover Condition settings. There can be multiple failover conditions inside AutomaticInputFailoverSettings.
--
-- /See:/ 'failoverCondition' smart constructor.
newtype FailoverCondition = FailoverCondition'
  { _fcFailoverConditionSettings ::
      Maybe FailoverConditionSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailoverCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcFailoverConditionSettings' - Failover condition type-specific settings.
failoverCondition ::
  FailoverCondition
failoverCondition =
  FailoverCondition' {_fcFailoverConditionSettings = Nothing}

-- | Failover condition type-specific settings.
fcFailoverConditionSettings :: Lens' FailoverCondition (Maybe FailoverConditionSettings)
fcFailoverConditionSettings = lens _fcFailoverConditionSettings (\s a -> s {_fcFailoverConditionSettings = a})

instance FromJSON FailoverCondition where
  parseJSON =
    withObject
      "FailoverCondition"
      (\x -> FailoverCondition' <$> (x .:? "failoverConditionSettings"))

instance Hashable FailoverCondition

instance NFData FailoverCondition

instance ToJSON FailoverCondition where
  toJSON FailoverCondition' {..} =
    object
      ( catMaybes
          [ ("failoverConditionSettings" .=)
              <$> _fcFailoverConditionSettings
          ]
      )

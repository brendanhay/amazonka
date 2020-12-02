{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration where

import Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The information needed to configure a delta time session window.
--
--
--
-- /See:/ 'lateDataRuleConfiguration' smart constructor.
newtype LateDataRuleConfiguration = LateDataRuleConfiguration'
  { _ldrcDeltaTimeSessionWindowConfiguration ::
      Maybe
        DeltaTimeSessionWindowConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LateDataRuleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrcDeltaTimeSessionWindowConfiguration' - The information needed to configure a delta time session window.
lateDataRuleConfiguration ::
  LateDataRuleConfiguration
lateDataRuleConfiguration =
  LateDataRuleConfiguration'
    { _ldrcDeltaTimeSessionWindowConfiguration =
        Nothing
    }

-- | The information needed to configure a delta time session window.
ldrcDeltaTimeSessionWindowConfiguration :: Lens' LateDataRuleConfiguration (Maybe DeltaTimeSessionWindowConfiguration)
ldrcDeltaTimeSessionWindowConfiguration = lens _ldrcDeltaTimeSessionWindowConfiguration (\s a -> s {_ldrcDeltaTimeSessionWindowConfiguration = a})

instance FromJSON LateDataRuleConfiguration where
  parseJSON =
    withObject
      "LateDataRuleConfiguration"
      ( \x ->
          LateDataRuleConfiguration'
            <$> (x .:? "deltaTimeSessionWindowConfiguration")
      )

instance Hashable LateDataRuleConfiguration

instance NFData LateDataRuleConfiguration

instance ToJSON LateDataRuleConfiguration where
  toJSON LateDataRuleConfiguration' {..} =
    object
      ( catMaybes
          [ ("deltaTimeSessionWindowConfiguration" .=)
              <$> _ldrcDeltaTimeSessionWindowConfiguration
          ]
      )

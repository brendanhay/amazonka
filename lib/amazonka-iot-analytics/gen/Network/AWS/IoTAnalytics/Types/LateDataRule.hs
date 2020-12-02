{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRule where

import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains the name and configuration information of a late data rule.
--
--
--
-- /See:/ 'lateDataRule' smart constructor.
data LateDataRule = LateDataRule'
  { _ldrRuleName :: !(Maybe Text),
    _ldrRuleConfiguration :: !LateDataRuleConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LateDataRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrRuleName' - The name of the late data rule.
--
-- * 'ldrRuleConfiguration' - The information needed to configure the late data rule.
lateDataRule ::
  -- | 'ldrRuleConfiguration'
  LateDataRuleConfiguration ->
  LateDataRule
lateDataRule pRuleConfiguration_ =
  LateDataRule'
    { _ldrRuleName = Nothing,
      _ldrRuleConfiguration = pRuleConfiguration_
    }

-- | The name of the late data rule.
ldrRuleName :: Lens' LateDataRule (Maybe Text)
ldrRuleName = lens _ldrRuleName (\s a -> s {_ldrRuleName = a})

-- | The information needed to configure the late data rule.
ldrRuleConfiguration :: Lens' LateDataRule LateDataRuleConfiguration
ldrRuleConfiguration = lens _ldrRuleConfiguration (\s a -> s {_ldrRuleConfiguration = a})

instance FromJSON LateDataRule where
  parseJSON =
    withObject
      "LateDataRule"
      ( \x ->
          LateDataRule'
            <$> (x .:? "ruleName") <*> (x .: "ruleConfiguration")
      )

instance Hashable LateDataRule

instance NFData LateDataRule

instance ToJSON LateDataRule where
  toJSON LateDataRule' {..} =
    object
      ( catMaybes
          [ ("ruleName" .=) <$> _ldrRuleName,
            Just ("ruleConfiguration" .= _ldrRuleConfiguration)
          ]
      )

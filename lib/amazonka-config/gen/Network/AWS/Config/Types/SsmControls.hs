{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.SsmControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SsmControls where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | AWS Systems Manager (SSM) specific remediation controls.
--
--
--
-- /See:/ 'ssmControls' smart constructor.
data SsmControls = SsmControls'
  { _scConcurrentExecutionRatePercentage ::
      !(Maybe Nat),
    _scErrorPercentage :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SsmControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scConcurrentExecutionRatePercentage' - The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
--
-- * 'scErrorPercentage' - The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
ssmControls ::
  SsmControls
ssmControls =
  SsmControls'
    { _scConcurrentExecutionRatePercentage = Nothing,
      _scErrorPercentage = Nothing
    }

-- | The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
scConcurrentExecutionRatePercentage :: Lens' SsmControls (Maybe Natural)
scConcurrentExecutionRatePercentage = lens _scConcurrentExecutionRatePercentage (\s a -> s {_scConcurrentExecutionRatePercentage = a}) . mapping _Nat

-- | The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
scErrorPercentage :: Lens' SsmControls (Maybe Natural)
scErrorPercentage = lens _scErrorPercentage (\s a -> s {_scErrorPercentage = a}) . mapping _Nat

instance FromJSON SsmControls where
  parseJSON =
    withObject
      "SsmControls"
      ( \x ->
          SsmControls'
            <$> (x .:? "ConcurrentExecutionRatePercentage")
            <*> (x .:? "ErrorPercentage")
      )

instance Hashable SsmControls

instance NFData SsmControls

instance ToJSON SsmControls where
  toJSON SsmControls' {..} =
    object
      ( catMaybes
          [ ("ConcurrentExecutionRatePercentage" .=)
              <$> _scConcurrentExecutionRatePercentage,
            ("ErrorPercentage" .=) <$> _scErrorPercentage
          ]
      )

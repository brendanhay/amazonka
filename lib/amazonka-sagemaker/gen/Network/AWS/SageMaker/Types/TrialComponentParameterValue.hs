{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentParameterValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value of a hyperparameter. Only one of @NumberValue@ or @StringValue@ can be specified.
--
--
-- This object is specified in the 'CreateTrialComponent' request.
--
--
-- /See:/ 'trialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { _tcpvNumberValue ::
      !(Maybe Double),
    _tcpvStringValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialComponentParameterValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcpvNumberValue' - The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
--
-- * 'tcpvStringValue' - The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
trialComponentParameterValue ::
  TrialComponentParameterValue
trialComponentParameterValue =
  TrialComponentParameterValue'
    { _tcpvNumberValue = Nothing,
      _tcpvStringValue = Nothing
    }

-- | The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
tcpvNumberValue :: Lens' TrialComponentParameterValue (Maybe Double)
tcpvNumberValue = lens _tcpvNumberValue (\s a -> s {_tcpvNumberValue = a})

-- | The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
tcpvStringValue :: Lens' TrialComponentParameterValue (Maybe Text)
tcpvStringValue = lens _tcpvStringValue (\s a -> s {_tcpvStringValue = a})

instance FromJSON TrialComponentParameterValue where
  parseJSON =
    withObject
      "TrialComponentParameterValue"
      ( \x ->
          TrialComponentParameterValue'
            <$> (x .:? "NumberValue") <*> (x .:? "StringValue")
      )

instance Hashable TrialComponentParameterValue

instance NFData TrialComponentParameterValue

instance ToJSON TrialComponentParameterValue where
  toJSON TrialComponentParameterValue' {..} =
    object
      ( catMaybes
          [ ("NumberValue" .=) <$> _tcpvNumberValue,
            ("StringValue" .=) <$> _tcpvStringValue
          ]
      )

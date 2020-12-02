{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the possible values for an integer hyperparameter.
--
--
--
-- /See:/ 'integerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { _iprsMinValue ::
      !Text,
    _iprsMaxValue ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntegerParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprsMinValue' - The minimum integer value allowed.
--
-- * 'iprsMaxValue' - The maximum integer value allowed.
integerParameterRangeSpecification ::
  -- | 'iprsMinValue'
  Text ->
  -- | 'iprsMaxValue'
  Text ->
  IntegerParameterRangeSpecification
integerParameterRangeSpecification pMinValue_ pMaxValue_ =
  IntegerParameterRangeSpecification'
    { _iprsMinValue = pMinValue_,
      _iprsMaxValue = pMaxValue_
    }

-- | The minimum integer value allowed.
iprsMinValue :: Lens' IntegerParameterRangeSpecification Text
iprsMinValue = lens _iprsMinValue (\s a -> s {_iprsMinValue = a})

-- | The maximum integer value allowed.
iprsMaxValue :: Lens' IntegerParameterRangeSpecification Text
iprsMaxValue = lens _iprsMaxValue (\s a -> s {_iprsMaxValue = a})

instance FromJSON IntegerParameterRangeSpecification where
  parseJSON =
    withObject
      "IntegerParameterRangeSpecification"
      ( \x ->
          IntegerParameterRangeSpecification'
            <$> (x .: "MinValue") <*> (x .: "MaxValue")
      )

instance Hashable IntegerParameterRangeSpecification

instance NFData IntegerParameterRangeSpecification

instance ToJSON IntegerParameterRangeSpecification where
  toJSON IntegerParameterRangeSpecification' {..} =
    object
      ( catMaybes
          [ Just ("MinValue" .= _iprsMinValue),
            Just ("MaxValue" .= _iprsMaxValue)
          ]
      )

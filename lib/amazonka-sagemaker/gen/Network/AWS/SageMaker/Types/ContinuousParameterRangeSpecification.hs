{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the possible values for a continuous hyperparameter.
--
--
--
-- /See:/ 'continuousParameterRangeSpecification' smart constructor.
data ContinuousParameterRangeSpecification = ContinuousParameterRangeSpecification'
  { _cprsMinValue ::
      !Text,
    _cprsMaxValue ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContinuousParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsMinValue' - The minimum floating-point value allowed.
--
-- * 'cprsMaxValue' - The maximum floating-point value allowed.
continuousParameterRangeSpecification ::
  -- | 'cprsMinValue'
  Text ->
  -- | 'cprsMaxValue'
  Text ->
  ContinuousParameterRangeSpecification
continuousParameterRangeSpecification pMinValue_ pMaxValue_ =
  ContinuousParameterRangeSpecification'
    { _cprsMinValue =
        pMinValue_,
      _cprsMaxValue = pMaxValue_
    }

-- | The minimum floating-point value allowed.
cprsMinValue :: Lens' ContinuousParameterRangeSpecification Text
cprsMinValue = lens _cprsMinValue (\s a -> s {_cprsMinValue = a})

-- | The maximum floating-point value allowed.
cprsMaxValue :: Lens' ContinuousParameterRangeSpecification Text
cprsMaxValue = lens _cprsMaxValue (\s a -> s {_cprsMaxValue = a})

instance FromJSON ContinuousParameterRangeSpecification where
  parseJSON =
    withObject
      "ContinuousParameterRangeSpecification"
      ( \x ->
          ContinuousParameterRangeSpecification'
            <$> (x .: "MinValue") <*> (x .: "MaxValue")
      )

instance Hashable ContinuousParameterRangeSpecification

instance NFData ContinuousParameterRangeSpecification

instance ToJSON ContinuousParameterRangeSpecification where
  toJSON ContinuousParameterRangeSpecification' {..} =
    object
      ( catMaybes
          [ Just ("MinValue" .= _cprsMinValue),
            Just ("MaxValue" .= _cprsMaxValue)
          ]
      )

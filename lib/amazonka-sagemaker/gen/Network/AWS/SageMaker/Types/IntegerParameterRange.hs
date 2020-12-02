{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | For a hyperparameter of the integer type, specifies the range that a hyperparameter tuning job searches.
--
--
--
-- /See:/ 'integerParameterRange' smart constructor.
data IntegerParameterRange = IntegerParameterRange'
  { _iprScalingType ::
      !(Maybe HyperParameterScalingType),
    _iprName :: !Text,
    _iprMinValue :: !Text,
    _iprMaxValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntegerParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprScalingType' - The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.
--
-- * 'iprName' - The name of the hyperparameter to search.
--
-- * 'iprMinValue' - The minimum value of the hyperparameter to search.
--
-- * 'iprMaxValue' - The maximum value of the hyperparameter to search.
integerParameterRange ::
  -- | 'iprName'
  Text ->
  -- | 'iprMinValue'
  Text ->
  -- | 'iprMaxValue'
  Text ->
  IntegerParameterRange
integerParameterRange pName_ pMinValue_ pMaxValue_ =
  IntegerParameterRange'
    { _iprScalingType = Nothing,
      _iprName = pName_,
      _iprMinValue = pMinValue_,
      _iprMaxValue = pMaxValue_
    }

-- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.
iprScalingType :: Lens' IntegerParameterRange (Maybe HyperParameterScalingType)
iprScalingType = lens _iprScalingType (\s a -> s {_iprScalingType = a})

-- | The name of the hyperparameter to search.
iprName :: Lens' IntegerParameterRange Text
iprName = lens _iprName (\s a -> s {_iprName = a})

-- | The minimum value of the hyperparameter to search.
iprMinValue :: Lens' IntegerParameterRange Text
iprMinValue = lens _iprMinValue (\s a -> s {_iprMinValue = a})

-- | The maximum value of the hyperparameter to search.
iprMaxValue :: Lens' IntegerParameterRange Text
iprMaxValue = lens _iprMaxValue (\s a -> s {_iprMaxValue = a})

instance FromJSON IntegerParameterRange where
  parseJSON =
    withObject
      "IntegerParameterRange"
      ( \x ->
          IntegerParameterRange'
            <$> (x .:? "ScalingType")
            <*> (x .: "Name")
            <*> (x .: "MinValue")
            <*> (x .: "MaxValue")
      )

instance Hashable IntegerParameterRange

instance NFData IntegerParameterRange

instance ToJSON IntegerParameterRange where
  toJSON IntegerParameterRange' {..} =
    object
      ( catMaybes
          [ ("ScalingType" .=) <$> _iprScalingType,
            Just ("Name" .= _iprName),
            Just ("MinValue" .= _iprMinValue),
            Just ("MaxValue" .= _iprMaxValue)
          ]
      )

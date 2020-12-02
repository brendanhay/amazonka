{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContinuousParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContinuousParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | A list of continuous hyperparameters to tune.
--
--
--
-- /See:/ 'continuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { _cScalingType ::
      !(Maybe HyperParameterScalingType),
    _cName :: !Text,
    _cMinValue :: !Text,
    _cMaxValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContinuousParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cScalingType' - The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.     * ReverseLogarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale. Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
--
-- * 'cName' - The name of the continuous hyperparameter to tune.
--
-- * 'cMinValue' - The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
--
-- * 'cMaxValue' - The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
continuousParameterRange ::
  -- | 'cName'
  Text ->
  -- | 'cMinValue'
  Text ->
  -- | 'cMaxValue'
  Text ->
  ContinuousParameterRange
continuousParameterRange pName_ pMinValue_ pMaxValue_ =
  ContinuousParameterRange'
    { _cScalingType = Nothing,
      _cName = pName_,
      _cMinValue = pMinValue_,
      _cMaxValue = pMaxValue_
    }

-- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.     * ReverseLogarithmic    * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale. Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
cScalingType :: Lens' ContinuousParameterRange (Maybe HyperParameterScalingType)
cScalingType = lens _cScalingType (\s a -> s {_cScalingType = a})

-- | The name of the continuous hyperparameter to tune.
cName :: Lens' ContinuousParameterRange Text
cName = lens _cName (\s a -> s {_cName = a})

-- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
cMinValue :: Lens' ContinuousParameterRange Text
cMinValue = lens _cMinValue (\s a -> s {_cMinValue = a})

-- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
cMaxValue :: Lens' ContinuousParameterRange Text
cMaxValue = lens _cMaxValue (\s a -> s {_cMaxValue = a})

instance FromJSON ContinuousParameterRange where
  parseJSON =
    withObject
      "ContinuousParameterRange"
      ( \x ->
          ContinuousParameterRange'
            <$> (x .:? "ScalingType")
            <*> (x .: "Name")
            <*> (x .: "MinValue")
            <*> (x .: "MaxValue")
      )

instance Hashable ContinuousParameterRange

instance NFData ContinuousParameterRange

instance ToJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange' {..} =
    object
      ( catMaybes
          [ ("ScalingType" .=) <$> _cScalingType,
            Just ("Name" .= _cName),
            Just ("MinValue" .= _cMinValue),
            Just ("MaxValue" .= _cMaxValue)
          ]
      )

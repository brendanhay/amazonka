{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TargetTrackingConfiguration where

import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a target tracking scaling policy configuration to use with Amazon EC2 Auto Scaling.
--
--
--
-- /See:/ 'targetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { _ttcPredefinedMetricSpecification ::
      !( Maybe
           PredefinedMetricSpecification
       ),
    _ttcCustomizedMetricSpecification ::
      !( Maybe
           CustomizedMetricSpecification
       ),
    _ttcDisableScaleIn :: !(Maybe Bool),
    _ttcTargetValue :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttcPredefinedMetricSpecification' - A predefined metric. You must specify either a predefined metric or a customized metric.
--
-- * 'ttcCustomizedMetricSpecification' - A customized metric. You must specify either a predefined metric or a customized metric.
--
-- * 'ttcDisableScaleIn' - Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
--
-- * 'ttcTargetValue' - The target value for the metric.
targetTrackingConfiguration ::
  -- | 'ttcTargetValue'
  Double ->
  TargetTrackingConfiguration
targetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { _ttcPredefinedMetricSpecification =
        Nothing,
      _ttcCustomizedMetricSpecification = Nothing,
      _ttcDisableScaleIn = Nothing,
      _ttcTargetValue = pTargetValue_
    }

-- | A predefined metric. You must specify either a predefined metric or a customized metric.
ttcPredefinedMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe PredefinedMetricSpecification)
ttcPredefinedMetricSpecification = lens _ttcPredefinedMetricSpecification (\s a -> s {_ttcPredefinedMetricSpecification = a})

-- | A customized metric. You must specify either a predefined metric or a customized metric.
ttcCustomizedMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe CustomizedMetricSpecification)
ttcCustomizedMetricSpecification = lens _ttcCustomizedMetricSpecification (\s a -> s {_ttcCustomizedMetricSpecification = a})

-- | Indicates whether scaling in by the target tracking scaling policy is disabled. If scaling in is disabled, the target tracking scaling policy doesn't remove instances from the Auto Scaling group. Otherwise, the target tracking scaling policy can remove instances from the Auto Scaling group. The default is @false@ .
ttcDisableScaleIn :: Lens' TargetTrackingConfiguration (Maybe Bool)
ttcDisableScaleIn = lens _ttcDisableScaleIn (\s a -> s {_ttcDisableScaleIn = a})

-- | The target value for the metric.
ttcTargetValue :: Lens' TargetTrackingConfiguration Double
ttcTargetValue = lens _ttcTargetValue (\s a -> s {_ttcTargetValue = a})

instance FromXML TargetTrackingConfiguration where
  parseXML x =
    TargetTrackingConfiguration'
      <$> (x .@? "PredefinedMetricSpecification")
      <*> (x .@? "CustomizedMetricSpecification")
      <*> (x .@? "DisableScaleIn")
      <*> (x .@ "TargetValue")

instance Hashable TargetTrackingConfiguration

instance NFData TargetTrackingConfiguration

instance ToQuery TargetTrackingConfiguration where
  toQuery TargetTrackingConfiguration' {..} =
    mconcat
      [ "PredefinedMetricSpecification"
          =: _ttcPredefinedMetricSpecification,
        "CustomizedMetricSpecification"
          =: _ttcCustomizedMetricSpecification,
        "DisableScaleIn" =: _ttcDisableScaleIn,
        "TargetValue" =: _ttcTargetValue
      ]

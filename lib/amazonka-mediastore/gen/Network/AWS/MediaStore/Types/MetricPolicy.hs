{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MetricPolicy where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types.ContainerLevelMetrics
import Network.AWS.MediaStore.Types.MetricPolicyRule
import Network.AWS.Prelude

-- | The metric policy that is associated with the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include rules to define groups of objects that you want MediaStore to send object-level metrics for.
--
--
-- To view examples of how to construct a metric policy for your use case, see <https://docs.aws.amazon.com/mediastore/latest/ug/policies-metric-examples.html Example Metric Policies> .
--
--
-- /See:/ 'metricPolicy' smart constructor.
data MetricPolicy = MetricPolicy'
  { _mpMetricPolicyRules ::
      !(Maybe (List1 MetricPolicyRule)),
    _mpContainerLevelMetrics :: !ContainerLevelMetrics
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpMetricPolicyRules' - A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
-- * 'mpContainerLevelMetrics' - A setting to enable or disable metrics at the container level.
metricPolicy ::
  -- | 'mpContainerLevelMetrics'
  ContainerLevelMetrics ->
  MetricPolicy
metricPolicy pContainerLevelMetrics_ =
  MetricPolicy'
    { _mpMetricPolicyRules = Nothing,
      _mpContainerLevelMetrics = pContainerLevelMetrics_
    }

-- | A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
mpMetricPolicyRules :: Lens' MetricPolicy (Maybe (NonEmpty MetricPolicyRule))
mpMetricPolicyRules = lens _mpMetricPolicyRules (\s a -> s {_mpMetricPolicyRules = a}) . mapping _List1

-- | A setting to enable or disable metrics at the container level.
mpContainerLevelMetrics :: Lens' MetricPolicy ContainerLevelMetrics
mpContainerLevelMetrics = lens _mpContainerLevelMetrics (\s a -> s {_mpContainerLevelMetrics = a})

instance FromJSON MetricPolicy where
  parseJSON =
    withObject
      "MetricPolicy"
      ( \x ->
          MetricPolicy'
            <$> (x .:? "MetricPolicyRules") <*> (x .: "ContainerLevelMetrics")
      )

instance Hashable MetricPolicy

instance NFData MetricPolicy

instance ToJSON MetricPolicy where
  toJSON MetricPolicy' {..} =
    object
      ( catMaybes
          [ ("MetricPolicyRules" .=) <$> _mpMetricPolicyRules,
            Just ("ContainerLevelMetrics" .= _mpContainerLevelMetrics)
          ]
      )

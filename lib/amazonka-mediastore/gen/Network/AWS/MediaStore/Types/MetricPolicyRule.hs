{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MetricPolicyRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MetricPolicyRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A setting that enables metrics at the object level. Each rule contains an object group and an object group name. If the policy includes the MetricPolicyRules parameter, you must include at least one rule. Each metric policy can include up to five rules by default. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
--
--
-- /See:/ 'metricPolicyRule' smart constructor.
data MetricPolicyRule = MetricPolicyRule'
  { _mprObjectGroup :: !Text,
    _mprObjectGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricPolicyRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprObjectGroup' - A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
--
-- * 'mprObjectGroupName' - A name that allows you to refer to the object group.
metricPolicyRule ::
  -- | 'mprObjectGroup'
  Text ->
  -- | 'mprObjectGroupName'
  Text ->
  MetricPolicyRule
metricPolicyRule pObjectGroup_ pObjectGroupName_ =
  MetricPolicyRule'
    { _mprObjectGroup = pObjectGroup_,
      _mprObjectGroupName = pObjectGroupName_
    }

-- | A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
mprObjectGroup :: Lens' MetricPolicyRule Text
mprObjectGroup = lens _mprObjectGroup (\s a -> s {_mprObjectGroup = a})

-- | A name that allows you to refer to the object group.
mprObjectGroupName :: Lens' MetricPolicyRule Text
mprObjectGroupName = lens _mprObjectGroupName (\s a -> s {_mprObjectGroupName = a})

instance FromJSON MetricPolicyRule where
  parseJSON =
    withObject
      "MetricPolicyRule"
      ( \x ->
          MetricPolicyRule'
            <$> (x .: "ObjectGroup") <*> (x .: "ObjectGroupName")
      )

instance Hashable MetricPolicyRule

instance NFData MetricPolicyRule

instance ToJSON MetricPolicyRule where
  toJSON MetricPolicyRule' {..} =
    object
      ( catMaybes
          [ Just ("ObjectGroup" .= _mprObjectGroup),
            Just ("ObjectGroupName" .= _mprObjectGroupName)
          ]
      )

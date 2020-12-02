{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupStickinessConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the target group stickiness for a rule.
--
--
--
-- /See:/ 'targetGroupStickinessConfig' smart constructor.
data TargetGroupStickinessConfig = TargetGroupStickinessConfig'
  { _tgscEnabled ::
      !(Maybe Bool),
    _tgscDurationSeconds ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroupStickinessConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgscEnabled' - Indicates whether target group stickiness is enabled.
--
-- * 'tgscDurationSeconds' - The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
targetGroupStickinessConfig ::
  TargetGroupStickinessConfig
targetGroupStickinessConfig =
  TargetGroupStickinessConfig'
    { _tgscEnabled = Nothing,
      _tgscDurationSeconds = Nothing
    }

-- | Indicates whether target group stickiness is enabled.
tgscEnabled :: Lens' TargetGroupStickinessConfig (Maybe Bool)
tgscEnabled = lens _tgscEnabled (\s a -> s {_tgscEnabled = a})

-- | The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
tgscDurationSeconds :: Lens' TargetGroupStickinessConfig (Maybe Int)
tgscDurationSeconds = lens _tgscDurationSeconds (\s a -> s {_tgscDurationSeconds = a})

instance FromXML TargetGroupStickinessConfig where
  parseXML x =
    TargetGroupStickinessConfig'
      <$> (x .@? "Enabled") <*> (x .@? "DurationSeconds")

instance Hashable TargetGroupStickinessConfig

instance NFData TargetGroupStickinessConfig

instance ToQuery TargetGroupStickinessConfig where
  toQuery TargetGroupStickinessConfig' {..} =
    mconcat
      [ "Enabled" =: _tgscEnabled,
        "DurationSeconds" =: _tgscDurationSeconds
      ]

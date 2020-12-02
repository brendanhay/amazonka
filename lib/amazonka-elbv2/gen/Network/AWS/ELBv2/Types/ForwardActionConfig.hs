{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ForwardActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ForwardActionConfig where

import Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
import Network.AWS.ELBv2.Types.TargetGroupTuple
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a forward action.
--
--
--
-- /See:/ 'forwardActionConfig' smart constructor.
data ForwardActionConfig = ForwardActionConfig'
  { _facTargetGroups ::
      !(Maybe [TargetGroupTuple]),
    _facTargetGroupStickinessConfig ::
      !(Maybe TargetGroupStickinessConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ForwardActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'facTargetGroups' - One or more target groups. For Network Load Balancers, you can specify a single target group.
--
-- * 'facTargetGroupStickinessConfig' - The target group stickiness for the rule.
forwardActionConfig ::
  ForwardActionConfig
forwardActionConfig =
  ForwardActionConfig'
    { _facTargetGroups = Nothing,
      _facTargetGroupStickinessConfig = Nothing
    }

-- | One or more target groups. For Network Load Balancers, you can specify a single target group.
facTargetGroups :: Lens' ForwardActionConfig [TargetGroupTuple]
facTargetGroups = lens _facTargetGroups (\s a -> s {_facTargetGroups = a}) . _Default . _Coerce

-- | The target group stickiness for the rule.
facTargetGroupStickinessConfig :: Lens' ForwardActionConfig (Maybe TargetGroupStickinessConfig)
facTargetGroupStickinessConfig = lens _facTargetGroupStickinessConfig (\s a -> s {_facTargetGroupStickinessConfig = a})

instance FromXML ForwardActionConfig where
  parseXML x =
    ForwardActionConfig'
      <$> (x .@? "TargetGroups" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TargetGroupStickinessConfig")

instance Hashable ForwardActionConfig

instance NFData ForwardActionConfig

instance ToQuery ForwardActionConfig where
  toQuery ForwardActionConfig' {..} =
    mconcat
      [ "TargetGroups"
          =: toQuery (toQueryList "member" <$> _facTargetGroups),
        "TargetGroupStickinessConfig" =: _facTargetGroupStickinessConfig
      ]

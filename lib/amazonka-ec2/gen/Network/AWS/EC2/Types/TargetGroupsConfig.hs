{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroupsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroupsConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TargetGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the target groups to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these target groups.
--
--
--
-- /See:/ 'targetGroupsConfig' smart constructor.
newtype TargetGroupsConfig = TargetGroupsConfig'
  { _tgcTargetGroups ::
      Maybe (List1 TargetGroup)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroupsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgcTargetGroups' - One or more target groups.
targetGroupsConfig ::
  TargetGroupsConfig
targetGroupsConfig =
  TargetGroupsConfig' {_tgcTargetGroups = Nothing}

-- | One or more target groups.
tgcTargetGroups :: Lens' TargetGroupsConfig (Maybe (NonEmpty TargetGroup))
tgcTargetGroups = lens _tgcTargetGroups (\s a -> s {_tgcTargetGroups = a}) . mapping _List1

instance FromXML TargetGroupsConfig where
  parseXML x =
    TargetGroupsConfig'
      <$> (x .@? "targetGroups" .!@ mempty >>= may (parseXMLList1 "item"))

instance Hashable TargetGroupsConfig

instance NFData TargetGroupsConfig

instance ToQuery TargetGroupsConfig where
  toQuery TargetGroupsConfig' {..} =
    mconcat
      [toQuery (toQueryList "TargetGroups" <$> _tgcTargetGroups)]

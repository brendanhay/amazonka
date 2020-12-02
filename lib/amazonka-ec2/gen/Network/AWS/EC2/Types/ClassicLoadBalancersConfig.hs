{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancersConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClassicLoadBalancer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these Classic Load Balancers.
--
--
--
-- /See:/ 'classicLoadBalancersConfig' smart constructor.
newtype ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { _clbcClassicLoadBalancers ::
      Maybe (List1 ClassicLoadBalancer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassicLoadBalancersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbcClassicLoadBalancers' - One or more Classic Load Balancers.
classicLoadBalancersConfig ::
  ClassicLoadBalancersConfig
classicLoadBalancersConfig =
  ClassicLoadBalancersConfig' {_clbcClassicLoadBalancers = Nothing}

-- | One or more Classic Load Balancers.
clbcClassicLoadBalancers :: Lens' ClassicLoadBalancersConfig (Maybe (NonEmpty ClassicLoadBalancer))
clbcClassicLoadBalancers = lens _clbcClassicLoadBalancers (\s a -> s {_clbcClassicLoadBalancers = a}) . mapping _List1

instance FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      <$> ( x .@? "classicLoadBalancers" .!@ mempty
              >>= may (parseXMLList1 "item")
          )

instance Hashable ClassicLoadBalancersConfig

instance NFData ClassicLoadBalancersConfig

instance ToQuery ClassicLoadBalancersConfig where
  toQuery ClassicLoadBalancersConfig' {..} =
    mconcat
      [ toQuery
          (toQueryList "ClassicLoadBalancers" <$> _clbcClassicLoadBalancers)
      ]

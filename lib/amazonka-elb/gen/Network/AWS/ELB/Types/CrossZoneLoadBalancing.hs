{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.CrossZoneLoadBalancing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.CrossZoneLoadBalancing where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
--
--
-- /See:/ 'crossZoneLoadBalancing' smart constructor.
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing'
  { _czlbEnabled ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CrossZoneLoadBalancing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'czlbEnabled' - Specifies whether cross-zone load balancing is enabled for the load balancer.
crossZoneLoadBalancing ::
  -- | 'czlbEnabled'
  Bool ->
  CrossZoneLoadBalancing
crossZoneLoadBalancing pEnabled_ =
  CrossZoneLoadBalancing' {_czlbEnabled = pEnabled_}

-- | Specifies whether cross-zone load balancing is enabled for the load balancer.
czlbEnabled :: Lens' CrossZoneLoadBalancing Bool
czlbEnabled = lens _czlbEnabled (\s a -> s {_czlbEnabled = a})

instance FromXML CrossZoneLoadBalancing where
  parseXML x = CrossZoneLoadBalancing' <$> (x .@ "Enabled")

instance Hashable CrossZoneLoadBalancing

instance NFData CrossZoneLoadBalancing

instance ToQuery CrossZoneLoadBalancing where
  toQuery CrossZoneLoadBalancing' {..} =
    mconcat ["Enabled" =: _czlbEnabled]

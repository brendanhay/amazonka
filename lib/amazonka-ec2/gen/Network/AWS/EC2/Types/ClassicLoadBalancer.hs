{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancer where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Classic Load Balancer.
--
--
--
-- /See:/ 'classicLoadBalancer' smart constructor.
newtype ClassicLoadBalancer = ClassicLoadBalancer'
  { _clbName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassicLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbName' - The name of the load balancer.
classicLoadBalancer ::
  ClassicLoadBalancer
classicLoadBalancer = ClassicLoadBalancer' {_clbName = Nothing}

-- | The name of the load balancer.
clbName :: Lens' ClassicLoadBalancer (Maybe Text)
clbName = lens _clbName (\s a -> s {_clbName = a})

instance FromXML ClassicLoadBalancer where
  parseXML x = ClassicLoadBalancer' <$> (x .@? "name")

instance Hashable ClassicLoadBalancer

instance NFData ClassicLoadBalancer

instance ToQuery ClassicLoadBalancer where
  toQuery ClassicLoadBalancer' {..} = mconcat ["Name" =: _clbName]

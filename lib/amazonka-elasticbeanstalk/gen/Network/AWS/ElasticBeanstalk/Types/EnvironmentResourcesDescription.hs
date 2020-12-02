{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription where

import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the AWS resources in use by this environment. This data is not live data.
--
--
--
-- /See:/ 'environmentResourcesDescription' smart constructor.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { _erdLoadBalancer ::
      Maybe
        LoadBalancerDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentResourcesDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdLoadBalancer' - Describes the LoadBalancer.
environmentResourcesDescription ::
  EnvironmentResourcesDescription
environmentResourcesDescription =
  EnvironmentResourcesDescription' {_erdLoadBalancer = Nothing}

-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\s a -> s {_erdLoadBalancer = a})

instance FromXML EnvironmentResourcesDescription where
  parseXML x =
    EnvironmentResourcesDescription' <$> (x .@? "LoadBalancer")

instance Hashable EnvironmentResourcesDescription

instance NFData EnvironmentResourcesDescription

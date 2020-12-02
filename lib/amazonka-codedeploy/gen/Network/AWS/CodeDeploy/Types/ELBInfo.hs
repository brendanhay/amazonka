{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ELBInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ELBInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.
--
--
--
-- /See:/ 'eLBInfo' smart constructor.
newtype ELBInfo = ELBInfo' {_elbiName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ELBInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elbiName' - For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
eLBInfo ::
  ELBInfo
eLBInfo = ELBInfo' {_elbiName = Nothing}

-- | For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
elbiName :: Lens' ELBInfo (Maybe Text)
elbiName = lens _elbiName (\s a -> s {_elbiName = a})

instance FromJSON ELBInfo where
  parseJSON =
    withObject "ELBInfo" (\x -> ELBInfo' <$> (x .:? "name"))

instance Hashable ELBInfo

instance NFData ELBInfo

instance ToJSON ELBInfo where
  toJSON ELBInfo' {..} =
    object (catMaybes [("name" .=) <$> _elbiName])

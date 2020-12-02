{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.NetworkConfiguration where

import Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure specifies the network configuration for an ECS task.
--
--
--
-- /See:/ 'networkConfiguration' smart constructor.
newtype NetworkConfiguration = NetworkConfiguration'
  { _ncAwsvpcConfiguration ::
      Maybe AWSVPCConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncAwsvpcConfiguration' - Use this structure to specify the VPC subnets and security groups for the task, and whether a public IP address is to be used. This structure is relevant only for ECS tasks that use the @awsvpc@ network mode.
networkConfiguration ::
  NetworkConfiguration
networkConfiguration =
  NetworkConfiguration' {_ncAwsvpcConfiguration = Nothing}

-- | Use this structure to specify the VPC subnets and security groups for the task, and whether a public IP address is to be used. This structure is relevant only for ECS tasks that use the @awsvpc@ network mode.
ncAwsvpcConfiguration :: Lens' NetworkConfiguration (Maybe AWSVPCConfiguration)
ncAwsvpcConfiguration = lens _ncAwsvpcConfiguration (\s a -> s {_ncAwsvpcConfiguration = a})

instance FromJSON NetworkConfiguration where
  parseJSON =
    withObject
      "NetworkConfiguration"
      (\x -> NetworkConfiguration' <$> (x .:? "awsvpcConfiguration"))

instance Hashable NetworkConfiguration

instance NFData NetworkConfiguration

instance ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    object
      (catMaybes [("awsvpcConfiguration" .=) <$> _ncAwsvpcConfiguration])

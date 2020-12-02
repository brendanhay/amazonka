{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monitoring for the instance.
--
--
--
-- /See:/ 'launchTemplatesMonitoringRequest' smart constructor.
newtype LaunchTemplatesMonitoringRequest = LaunchTemplatesMonitoringRequest'
  { _ltmrEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplatesMonitoringRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmrEnabled' - Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
launchTemplatesMonitoringRequest ::
  LaunchTemplatesMonitoringRequest
launchTemplatesMonitoringRequest =
  LaunchTemplatesMonitoringRequest' {_ltmrEnabled = Nothing}

-- | Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
ltmrEnabled :: Lens' LaunchTemplatesMonitoringRequest (Maybe Bool)
ltmrEnabled = lens _ltmrEnabled (\s a -> s {_ltmrEnabled = a})

instance Hashable LaunchTemplatesMonitoringRequest

instance NFData LaunchTemplatesMonitoringRequest

instance ToQuery LaunchTemplatesMonitoringRequest where
  toQuery LaunchTemplatesMonitoringRequest' {..} =
    mconcat ["Enabled" =: _ltmrEnabled]

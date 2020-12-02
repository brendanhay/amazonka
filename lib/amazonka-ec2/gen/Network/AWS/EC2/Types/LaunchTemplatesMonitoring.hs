{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoring where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monitoring for the instance.
--
--
--
-- /See:/ 'launchTemplatesMonitoring' smart constructor.
newtype LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
  { _ltmEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplatesMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmEnabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
launchTemplatesMonitoring ::
  LaunchTemplatesMonitoring
launchTemplatesMonitoring =
  LaunchTemplatesMonitoring' {_ltmEnabled = Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
ltmEnabled :: Lens' LaunchTemplatesMonitoring (Maybe Bool)
ltmEnabled = lens _ltmEnabled (\s a -> s {_ltmEnabled = a})

instance FromXML LaunchTemplatesMonitoring where
  parseXML x = LaunchTemplatesMonitoring' <$> (x .@? "enabled")

instance Hashable LaunchTemplatesMonitoring

instance NFData LaunchTemplatesMonitoring

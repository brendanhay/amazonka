{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetMonitoring where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether monitoring is enabled.
--
--
--
-- /See:/ 'spotFleetMonitoring' smart constructor.
newtype SpotFleetMonitoring = SpotFleetMonitoring'
  { _sfmEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotFleetMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfmEnabled' - Enables monitoring for the instance. Default: @false@
spotFleetMonitoring ::
  SpotFleetMonitoring
spotFleetMonitoring = SpotFleetMonitoring' {_sfmEnabled = Nothing}

-- | Enables monitoring for the instance. Default: @false@
sfmEnabled :: Lens' SpotFleetMonitoring (Maybe Bool)
sfmEnabled = lens _sfmEnabled (\s a -> s {_sfmEnabled = a})

instance FromXML SpotFleetMonitoring where
  parseXML x = SpotFleetMonitoring' <$> (x .@? "enabled")

instance Hashable SpotFleetMonitoring

instance NFData SpotFleetMonitoring

instance ToQuery SpotFleetMonitoring where
  toQuery SpotFleetMonitoring' {..} =
    mconcat ["Enabled" =: _sfmEnabled]

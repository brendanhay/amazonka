{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Monitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Monitoring where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.MonitoringState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'monitoring' smart constructor.
newtype Monitoring = Monitoring' {_mState :: Maybe MonitoringState}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Monitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
monitoring ::
  Monitoring
monitoring = Monitoring' {_mState = Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mState :: Lens' Monitoring (Maybe MonitoringState)
mState = lens _mState (\s a -> s {_mState = a})

instance FromXML Monitoring where
  parseXML x = Monitoring' <$> (x .@? "state")

instance Hashable Monitoring

instance NFData Monitoring

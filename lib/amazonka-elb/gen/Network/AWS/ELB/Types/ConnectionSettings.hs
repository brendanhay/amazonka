{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionSettings where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the @ConnectionSettings@ attribute.
--
--
--
-- /See:/ 'connectionSettings' smart constructor.
newtype ConnectionSettings = ConnectionSettings'
  { _csIdleTimeout ::
      Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csIdleTimeout' - The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
connectionSettings ::
  -- | 'csIdleTimeout'
  Natural ->
  ConnectionSettings
connectionSettings pIdleTimeout_ =
  ConnectionSettings' {_csIdleTimeout = _Nat # pIdleTimeout_}

-- | The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
csIdleTimeout :: Lens' ConnectionSettings Natural
csIdleTimeout = lens _csIdleTimeout (\s a -> s {_csIdleTimeout = a}) . _Nat

instance FromXML ConnectionSettings where
  parseXML x = ConnectionSettings' <$> (x .@ "IdleTimeout")

instance Hashable ConnectionSettings

instance NFData ConnectionSettings

instance ToQuery ConnectionSettings where
  toQuery ConnectionSettings' {..} =
    mconcat ["IdleTimeout" =: _csIdleTimeout]

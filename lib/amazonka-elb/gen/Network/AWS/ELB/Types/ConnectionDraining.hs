{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionDraining
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionDraining where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the @ConnectionDraining@ attribute.
--
--
--
-- /See:/ 'connectionDraining' smart constructor.
data ConnectionDraining = ConnectionDraining'
  { _cdTimeout ::
      !(Maybe Int),
    _cdEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionDraining' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdTimeout' - The maximum time, in seconds, to keep the existing connections open before deregistering the instances.
--
-- * 'cdEnabled' - Specifies whether connection draining is enabled for the load balancer.
connectionDraining ::
  -- | 'cdEnabled'
  Bool ->
  ConnectionDraining
connectionDraining pEnabled_ =
  ConnectionDraining' {_cdTimeout = Nothing, _cdEnabled = pEnabled_}

-- | The maximum time, in seconds, to keep the existing connections open before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Int)
cdTimeout = lens _cdTimeout (\s a -> s {_cdTimeout = a})

-- | Specifies whether connection draining is enabled for the load balancer.
cdEnabled :: Lens' ConnectionDraining Bool
cdEnabled = lens _cdEnabled (\s a -> s {_cdEnabled = a})

instance FromXML ConnectionDraining where
  parseXML x =
    ConnectionDraining' <$> (x .@? "Timeout") <*> (x .@ "Enabled")

instance Hashable ConnectionDraining

instance NFData ConnectionDraining

instance ToQuery ConnectionDraining where
  toQuery ConnectionDraining' {..} =
    mconcat ["Timeout" =: _cdTimeout, "Enabled" =: _cdEnabled]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CustomerNodeEndpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The endpoint from which data should be migrated.
--
--
--
-- /See:/ 'customerNodeEndpoint' smart constructor.
data CustomerNodeEndpoint = CustomerNodeEndpoint'
  { _cneAddress ::
      !(Maybe Text),
    _cnePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerNodeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cneAddress' - The address of the node endpoint
--
-- * 'cnePort' - The port of the node endpoint
customerNodeEndpoint ::
  CustomerNodeEndpoint
customerNodeEndpoint =
  CustomerNodeEndpoint' {_cneAddress = Nothing, _cnePort = Nothing}

-- | The address of the node endpoint
cneAddress :: Lens' CustomerNodeEndpoint (Maybe Text)
cneAddress = lens _cneAddress (\s a -> s {_cneAddress = a})

-- | The port of the node endpoint
cnePort :: Lens' CustomerNodeEndpoint (Maybe Int)
cnePort = lens _cnePort (\s a -> s {_cnePort = a})

instance Hashable CustomerNodeEndpoint

instance NFData CustomerNodeEndpoint

instance ToQuery CustomerNodeEndpoint where
  toQuery CustomerNodeEndpoint' {..} =
    mconcat ["Address" =: _cneAddress, "Port" =: _cnePort]

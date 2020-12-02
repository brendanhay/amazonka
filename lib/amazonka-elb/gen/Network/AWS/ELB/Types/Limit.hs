{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Limit where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
--
--
-- /See:/ 'limit' smart constructor.
data Limit = Limit'
  { _lMax :: !(Maybe Text),
    _lName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMax' - The maximum value of the limit.
--
-- * 'lName' - The name of the limit. The possible values are:     * classic-listeners     * classic-load-balancers     * classic-registered-instances
limit ::
  Limit
limit = Limit' {_lMax = Nothing, _lName = Nothing}

-- | The maximum value of the limit.
lMax :: Lens' Limit (Maybe Text)
lMax = lens _lMax (\s a -> s {_lMax = a})

-- | The name of the limit. The possible values are:     * classic-listeners     * classic-load-balancers     * classic-registered-instances
lName :: Lens' Limit (Maybe Text)
lName = lens _lName (\s a -> s {_lName = a})

instance FromXML Limit where
  parseXML x = Limit' <$> (x .@? "Max") <*> (x .@? "Name")

instance Hashable Limit

instance NFData Limit

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PortRange where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a range of ports.
--
--
--
-- /See:/ 'portRange' smart constructor.
data PortRange = PortRange'
  { _prTo :: !(Maybe Int),
    _prFrom :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prTo' - The last port in the range.
--
-- * 'prFrom' - The first port in the range.
portRange ::
  PortRange
portRange = PortRange' {_prTo = Nothing, _prFrom = Nothing}

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Int)
prTo = lens _prTo (\s a -> s {_prTo = a})

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Int)
prFrom = lens _prFrom (\s a -> s {_prFrom = a})

instance FromXML PortRange where
  parseXML x = PortRange' <$> (x .@? "to") <*> (x .@? "from")

instance Hashable PortRange

instance NFData PortRange

instance ToQuery PortRange where
  toQuery PortRange' {..} = mconcat ["To" =: _prTo, "From" =: _prFrom]

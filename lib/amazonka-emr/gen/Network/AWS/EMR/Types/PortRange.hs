{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PortRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of port ranges that are permitted to allow inbound traffic from all public IP addresses. To specify a single port, use the same value for @MinRange@ and @MaxRange@ .
--
--
--
-- /See:/ 'portRange' smart constructor.
data PortRange = PortRange'
  { _prMaxRange :: !(Maybe Int),
    _prMinRange :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prMaxRange' - The smallest port number in a specified range of port numbers.
--
-- * 'prMinRange' - The smallest port number in a specified range of port numbers.
portRange ::
  -- | 'prMinRange'
  Int ->
  PortRange
portRange pMinRange_ =
  PortRange' {_prMaxRange = Nothing, _prMinRange = pMinRange_}

-- | The smallest port number in a specified range of port numbers.
prMaxRange :: Lens' PortRange (Maybe Int)
prMaxRange = lens _prMaxRange (\s a -> s {_prMaxRange = a})

-- | The smallest port number in a specified range of port numbers.
prMinRange :: Lens' PortRange Int
prMinRange = lens _prMinRange (\s a -> s {_prMinRange = a})

instance FromJSON PortRange where
  parseJSON =
    withObject
      "PortRange"
      (\x -> PortRange' <$> (x .:? "MaxRange") <*> (x .: "MinRange"))

instance Hashable PortRange

instance NFData PortRange

instance ToJSON PortRange where
  toJSON PortRange' {..} =
    object
      ( catMaybes
          [ ("MaxRange" .=) <$> _prMaxRange,
            Just ("MinRange" .= _prMinRange)
          ]
      )

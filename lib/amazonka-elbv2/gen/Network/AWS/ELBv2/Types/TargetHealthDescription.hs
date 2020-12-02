{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthDescription where

import Network.AWS.ELBv2.Types.TargetDescription
import Network.AWS.ELBv2.Types.TargetHealth
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the health of a target.
--
--
--
-- /See:/ 'targetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { _thdTargetHealth ::
      !(Maybe TargetHealth),
    _thdHealthCheckPort :: !(Maybe Text),
    _thdTarget :: !(Maybe TargetDescription)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetHealthDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thdTargetHealth' - The health information for the target.
--
-- * 'thdHealthCheckPort' - The port to use to connect with the target.
--
-- * 'thdTarget' - The description of the target.
targetHealthDescription ::
  TargetHealthDescription
targetHealthDescription =
  TargetHealthDescription'
    { _thdTargetHealth = Nothing,
      _thdHealthCheckPort = Nothing,
      _thdTarget = Nothing
    }

-- | The health information for the target.
thdTargetHealth :: Lens' TargetHealthDescription (Maybe TargetHealth)
thdTargetHealth = lens _thdTargetHealth (\s a -> s {_thdTargetHealth = a})

-- | The port to use to connect with the target.
thdHealthCheckPort :: Lens' TargetHealthDescription (Maybe Text)
thdHealthCheckPort = lens _thdHealthCheckPort (\s a -> s {_thdHealthCheckPort = a})

-- | The description of the target.
thdTarget :: Lens' TargetHealthDescription (Maybe TargetDescription)
thdTarget = lens _thdTarget (\s a -> s {_thdTarget = a})

instance FromXML TargetHealthDescription where
  parseXML x =
    TargetHealthDescription'
      <$> (x .@? "TargetHealth")
      <*> (x .@? "HealthCheckPort")
      <*> (x .@? "Target")

instance Hashable TargetHealthDescription

instance NFData TargetHealthDescription

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealth where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState

-- | Information about the connection health of an RDS Proxy target.
--
--
--
-- /See:/ 'targetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { _thState :: !(Maybe TargetState),
    _thReason :: !(Maybe TargetHealthReason),
    _thDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thState' - The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:  @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
--
-- * 'thReason' - The reason for the current health @State@ of the RDS Proxy target.
--
-- * 'thDescription' - A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
targetHealth ::
  TargetHealth
targetHealth =
  TargetHealth'
    { _thState = Nothing,
      _thReason = Nothing,
      _thDescription = Nothing
    }

-- | The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:  @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
thState :: Lens' TargetHealth (Maybe TargetState)
thState = lens _thState (\s a -> s {_thState = a})

-- | The reason for the current health @State@ of the RDS Proxy target.
thReason :: Lens' TargetHealth (Maybe TargetHealthReason)
thReason = lens _thReason (\s a -> s {_thReason = a})

-- | A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
thDescription :: Lens' TargetHealth (Maybe Text)
thDescription = lens _thDescription (\s a -> s {_thDescription = a})

instance FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      <$> (x .@? "State") <*> (x .@? "Reason") <*> (x .@? "Description")

instance Hashable TargetHealth

instance NFData TargetHealth

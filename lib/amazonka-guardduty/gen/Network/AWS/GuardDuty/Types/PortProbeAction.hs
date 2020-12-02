{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeAction where

import Network.AWS.GuardDuty.Types.PortProbeDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the PORT_PROBE action described in the finding.
--
--
--
-- /See:/ 'portProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { _ppaPortProbeDetails ::
      !(Maybe [PortProbeDetail]),
    _ppaBlocked :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortProbeAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppaPortProbeDetails' - A list of objects related to port probe details.
--
-- * 'ppaBlocked' - Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
portProbeAction ::
  PortProbeAction
portProbeAction =
  PortProbeAction'
    { _ppaPortProbeDetails = Nothing,
      _ppaBlocked = Nothing
    }

-- | A list of objects related to port probe details.
ppaPortProbeDetails :: Lens' PortProbeAction [PortProbeDetail]
ppaPortProbeDetails = lens _ppaPortProbeDetails (\s a -> s {_ppaPortProbeDetails = a}) . _Default . _Coerce

-- | Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
ppaBlocked :: Lens' PortProbeAction (Maybe Bool)
ppaBlocked = lens _ppaBlocked (\s a -> s {_ppaBlocked = a})

instance FromJSON PortProbeAction where
  parseJSON =
    withObject
      "PortProbeAction"
      ( \x ->
          PortProbeAction'
            <$> (x .:? "portProbeDetails" .!= mempty) <*> (x .:? "blocked")
      )

instance Hashable PortProbeAction

instance NFData PortProbeAction

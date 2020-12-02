{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MovingAddressStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MovingAddressStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.MoveStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of a moving Elastic IP address.
--
--
--
-- /See:/ 'movingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { _masMoveStatus ::
      !(Maybe MoveStatus),
    _masPublicIP :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MovingAddressStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'masMoveStatus' - The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
--
-- * 'masPublicIP' - The Elastic IP address.
movingAddressStatus ::
  MovingAddressStatus
movingAddressStatus =
  MovingAddressStatus'
    { _masMoveStatus = Nothing,
      _masPublicIP = Nothing
    }

-- | The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
masMoveStatus :: Lens' MovingAddressStatus (Maybe MoveStatus)
masMoveStatus = lens _masMoveStatus (\s a -> s {_masMoveStatus = a})

-- | The Elastic IP address.
masPublicIP :: Lens' MovingAddressStatus (Maybe Text)
masPublicIP = lens _masPublicIP (\s a -> s {_masPublicIP = a})

instance FromXML MovingAddressStatus where
  parseXML x =
    MovingAddressStatus'
      <$> (x .@? "moveStatus") <*> (x .@? "publicIp")

instance Hashable MovingAddressStatus

instance NFData MovingAddressStatus

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNConnectionStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNConnectionStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of a client connection.
--
--
--
-- /See:/ 'clientVPNConnectionStatus' smart constructor.
data ClientVPNConnectionStatus = ClientVPNConnectionStatus'
  { _cvcsCode ::
      !(Maybe ClientVPNConnectionStatusCode),
    _cvcsMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcsCode' - The state of the client connection.
--
-- * 'cvcsMessage' - A message about the status of the client connection, if applicable.
clientVPNConnectionStatus ::
  ClientVPNConnectionStatus
clientVPNConnectionStatus =
  ClientVPNConnectionStatus'
    { _cvcsCode = Nothing,
      _cvcsMessage = Nothing
    }

-- | The state of the client connection.
cvcsCode :: Lens' ClientVPNConnectionStatus (Maybe ClientVPNConnectionStatusCode)
cvcsCode = lens _cvcsCode (\s a -> s {_cvcsCode = a})

-- | A message about the status of the client connection, if applicable.
cvcsMessage :: Lens' ClientVPNConnectionStatus (Maybe Text)
cvcsMessage = lens _cvcsMessage (\s a -> s {_cvcsMessage = a})

instance FromXML ClientVPNConnectionStatus where
  parseXML x =
    ClientVPNConnectionStatus'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable ClientVPNConnectionStatus

instance NFData ClientVPNConnectionStatus

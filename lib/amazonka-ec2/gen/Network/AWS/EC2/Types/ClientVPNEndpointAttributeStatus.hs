{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of the Client VPN endpoint attribute.
--
--
--
-- /See:/ 'clientVPNEndpointAttributeStatus' smart constructor.
data ClientVPNEndpointAttributeStatus = ClientVPNEndpointAttributeStatus'
  { _cveasCode ::
      !( Maybe
           ClientVPNEndpointAttributeStatusCode
       ),
    _cveasMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNEndpointAttributeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cveasCode' - The status code.
--
-- * 'cveasMessage' - The status message.
clientVPNEndpointAttributeStatus ::
  ClientVPNEndpointAttributeStatus
clientVPNEndpointAttributeStatus =
  ClientVPNEndpointAttributeStatus'
    { _cveasCode = Nothing,
      _cveasMessage = Nothing
    }

-- | The status code.
cveasCode :: Lens' ClientVPNEndpointAttributeStatus (Maybe ClientVPNEndpointAttributeStatusCode)
cveasCode = lens _cveasCode (\s a -> s {_cveasCode = a})

-- | The status message.
cveasMessage :: Lens' ClientVPNEndpointAttributeStatus (Maybe Text)
cveasMessage = lens _cveasMessage (\s a -> s {_cveasMessage = a})

instance FromXML ClientVPNEndpointAttributeStatus where
  parseXML x =
    ClientVPNEndpointAttributeStatus'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable ClientVPNEndpointAttributeStatus

instance NFData ClientVPNEndpointAttributeStatus

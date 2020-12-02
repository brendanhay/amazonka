{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectResponseOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The options for managing connection authorization for new client connections.
--
--
--
-- /See:/ 'clientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { _ccroStatus ::
      !( Maybe
           ClientVPNEndpointAttributeStatus
       ),
    _ccroEnabled :: !(Maybe Bool),
    _ccroLambdaFunctionARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientConnectResponseOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccroStatus' - The status of any updates to the client connect options.
--
-- * 'ccroEnabled' - Indicates whether client connect options are enabled.
--
-- * 'ccroLambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
clientConnectResponseOptions ::
  ClientConnectResponseOptions
clientConnectResponseOptions =
  ClientConnectResponseOptions'
    { _ccroStatus = Nothing,
      _ccroEnabled = Nothing,
      _ccroLambdaFunctionARN = Nothing
    }

-- | The status of any updates to the client connect options.
ccroStatus :: Lens' ClientConnectResponseOptions (Maybe ClientVPNEndpointAttributeStatus)
ccroStatus = lens _ccroStatus (\s a -> s {_ccroStatus = a})

-- | Indicates whether client connect options are enabled.
ccroEnabled :: Lens' ClientConnectResponseOptions (Maybe Bool)
ccroEnabled = lens _ccroEnabled (\s a -> s {_ccroEnabled = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
ccroLambdaFunctionARN :: Lens' ClientConnectResponseOptions (Maybe Text)
ccroLambdaFunctionARN = lens _ccroLambdaFunctionARN (\s a -> s {_ccroLambdaFunctionARN = a})

instance FromXML ClientConnectResponseOptions where
  parseXML x =
    ClientConnectResponseOptions'
      <$> (x .@? "status")
      <*> (x .@? "enabled")
      <*> (x .@? "lambdaFunctionArn")

instance Hashable ClientConnectResponseOptions

instance NFData ClientConnectResponseOptions

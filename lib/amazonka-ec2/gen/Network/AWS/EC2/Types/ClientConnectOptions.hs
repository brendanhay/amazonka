{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientConnectOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The options for managing connection authorization for new client connections.
--
--
--
-- /See:/ 'clientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { _ccoEnabled ::
      !(Maybe Bool),
    _ccoLambdaFunctionARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientConnectOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccoEnabled' - Indicates whether client connect options are enabled. The default is @false@ (not enabled).
--
-- * 'ccoLambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
clientConnectOptions ::
  ClientConnectOptions
clientConnectOptions =
  ClientConnectOptions'
    { _ccoEnabled = Nothing,
      _ccoLambdaFunctionARN = Nothing
    }

-- | Indicates whether client connect options are enabled. The default is @false@ (not enabled).
ccoEnabled :: Lens' ClientConnectOptions (Maybe Bool)
ccoEnabled = lens _ccoEnabled (\s a -> s {_ccoEnabled = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for connection authorization.
ccoLambdaFunctionARN :: Lens' ClientConnectOptions (Maybe Text)
ccoLambdaFunctionARN = lens _ccoLambdaFunctionARN (\s a -> s {_ccoLambdaFunctionARN = a})

instance Hashable ClientConnectOptions

instance NFData ClientConnectOptions

instance ToQuery ClientConnectOptions where
  toQuery ClientConnectOptions' {..} =
    mconcat
      [ "Enabled" =: _ccoEnabled,
        "LambdaFunctionArn" =: _ccoLambdaFunctionARN
      ]

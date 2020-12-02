{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters for a LAMBDA task type.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowLambdaParameters' smart constructor.
data MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters'
  { _mwlpPayload ::
      !( Maybe
           (Sensitive Base64)
       ),
    _mwlpQualifier ::
      !(Maybe Text),
    _mwlpClientContext ::
      !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowLambdaParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwlpPayload' - JSON to provide to your Lambda function as input.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mwlpQualifier' - (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
--
-- * 'mwlpClientContext' - Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
maintenanceWindowLambdaParameters ::
  MaintenanceWindowLambdaParameters
maintenanceWindowLambdaParameters =
  MaintenanceWindowLambdaParameters'
    { _mwlpPayload = Nothing,
      _mwlpQualifier = Nothing,
      _mwlpClientContext = Nothing
    }

-- | JSON to provide to your Lambda function as input.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mwlpPayload :: Lens' MaintenanceWindowLambdaParameters (Maybe ByteString)
mwlpPayload = lens _mwlpPayload (\s a -> s {_mwlpPayload = a}) . mapping (_Sensitive . _Base64)

-- | (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
mwlpQualifier :: Lens' MaintenanceWindowLambdaParameters (Maybe Text)
mwlpQualifier = lens _mwlpQualifier (\s a -> s {_mwlpQualifier = a})

-- | Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
mwlpClientContext :: Lens' MaintenanceWindowLambdaParameters (Maybe Text)
mwlpClientContext = lens _mwlpClientContext (\s a -> s {_mwlpClientContext = a})

instance FromJSON MaintenanceWindowLambdaParameters where
  parseJSON =
    withObject
      "MaintenanceWindowLambdaParameters"
      ( \x ->
          MaintenanceWindowLambdaParameters'
            <$> (x .:? "Payload")
            <*> (x .:? "Qualifier")
            <*> (x .:? "ClientContext")
      )

instance Hashable MaintenanceWindowLambdaParameters

instance NFData MaintenanceWindowLambdaParameters

instance ToJSON MaintenanceWindowLambdaParameters where
  toJSON MaintenanceWindowLambdaParameters' {..} =
    object
      ( catMaybes
          [ ("Payload" .=) <$> _mwlpPayload,
            ("Qualifier" .=) <$> _mwlpQualifier,
            ("ClientContext" .=) <$> _mwlpClientContext
          ]
      )

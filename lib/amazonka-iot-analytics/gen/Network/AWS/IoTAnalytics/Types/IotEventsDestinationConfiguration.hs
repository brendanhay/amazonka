{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
--
--
--
-- /See:/ 'iotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { _iedcInputName ::
      !Text,
    _iedcRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IotEventsDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iedcInputName' - The name of the AWS IoT Events input to which dataset contents are delivered.
--
-- * 'iedcRoleARN' - The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
iotEventsDestinationConfiguration ::
  -- | 'iedcInputName'
  Text ->
  -- | 'iedcRoleARN'
  Text ->
  IotEventsDestinationConfiguration
iotEventsDestinationConfiguration pInputName_ pRoleARN_ =
  IotEventsDestinationConfiguration'
    { _iedcInputName = pInputName_,
      _iedcRoleARN = pRoleARN_
    }

-- | The name of the AWS IoT Events input to which dataset contents are delivered.
iedcInputName :: Lens' IotEventsDestinationConfiguration Text
iedcInputName = lens _iedcInputName (\s a -> s {_iedcInputName = a})

-- | The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
iedcRoleARN :: Lens' IotEventsDestinationConfiguration Text
iedcRoleARN = lens _iedcRoleARN (\s a -> s {_iedcRoleARN = a})

instance FromJSON IotEventsDestinationConfiguration where
  parseJSON =
    withObject
      "IotEventsDestinationConfiguration"
      ( \x ->
          IotEventsDestinationConfiguration'
            <$> (x .: "inputName") <*> (x .: "roleArn")
      )

instance Hashable IotEventsDestinationConfiguration

instance NFData IotEventsDestinationConfiguration

instance ToJSON IotEventsDestinationConfiguration where
  toJSON IotEventsDestinationConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("inputName" .= _iedcInputName),
            Just ("roleArn" .= _iedcRoleARN)
          ]
      )

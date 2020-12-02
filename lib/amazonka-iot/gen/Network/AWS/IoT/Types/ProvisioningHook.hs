{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningHook where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains @payloadVersion@ and @targetArn@ .
--
--
--
-- /See:/ 'provisioningHook' smart constructor.
data ProvisioningHook = ProvisioningHook'
  { _phPayloadVersion ::
      !(Maybe Text),
    _phTargetARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phPayloadVersion' - The payload that was sent to the target function. /Note:/ Only Lambda functions are currently supported.
--
-- * 'phTargetARN' - The ARN of the target function. /Note:/ Only Lambda functions are currently supported.
provisioningHook ::
  -- | 'phTargetARN'
  Text ->
  ProvisioningHook
provisioningHook pTargetARN_ =
  ProvisioningHook'
    { _phPayloadVersion = Nothing,
      _phTargetARN = pTargetARN_
    }

-- | The payload that was sent to the target function. /Note:/ Only Lambda functions are currently supported.
phPayloadVersion :: Lens' ProvisioningHook (Maybe Text)
phPayloadVersion = lens _phPayloadVersion (\s a -> s {_phPayloadVersion = a})

-- | The ARN of the target function. /Note:/ Only Lambda functions are currently supported.
phTargetARN :: Lens' ProvisioningHook Text
phTargetARN = lens _phTargetARN (\s a -> s {_phTargetARN = a})

instance FromJSON ProvisioningHook where
  parseJSON =
    withObject
      "ProvisioningHook"
      ( \x ->
          ProvisioningHook'
            <$> (x .:? "payloadVersion") <*> (x .: "targetArn")
      )

instance Hashable ProvisioningHook

instance NFData ProvisioningHook

instance ToJSON ProvisioningHook where
  toJSON ProvisioningHook' {..} =
    object
      ( catMaybes
          [ ("payloadVersion" .=) <$> _phPayloadVersion,
            Just ("targetArn" .= _phTargetARN)
          ]
      )

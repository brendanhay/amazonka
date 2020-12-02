{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateDeviceCertificateParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateDeviceCertificateParams where

import Network.AWS.IoT.Types.DeviceCertificateUpdateAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
--
--
-- /See:/ 'updateDeviceCertificateParams' smart constructor.
newtype UpdateDeviceCertificateParams = UpdateDeviceCertificateParams'
  { _udcpAction ::
      DeviceCertificateUpdateAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDeviceCertificateParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcpAction' - The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
updateDeviceCertificateParams ::
  -- | 'udcpAction'
  DeviceCertificateUpdateAction ->
  UpdateDeviceCertificateParams
updateDeviceCertificateParams pAction_ =
  UpdateDeviceCertificateParams' {_udcpAction = pAction_}

-- | The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
udcpAction :: Lens' UpdateDeviceCertificateParams DeviceCertificateUpdateAction
udcpAction = lens _udcpAction (\s a -> s {_udcpAction = a})

instance FromJSON UpdateDeviceCertificateParams where
  parseJSON =
    withObject
      "UpdateDeviceCertificateParams"
      (\x -> UpdateDeviceCertificateParams' <$> (x .: "action"))

instance Hashable UpdateDeviceCertificateParams

instance NFData UpdateDeviceCertificateParams

instance ToJSON UpdateDeviceCertificateParams where
  toJSON UpdateDeviceCertificateParams' {..} =
    object (catMaybes [Just ("action" .= _udcpAction)])

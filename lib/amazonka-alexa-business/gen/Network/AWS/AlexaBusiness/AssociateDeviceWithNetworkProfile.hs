{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with the specified network profile.
module Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
  ( -- * Creating a Request
    associateDeviceWithNetworkProfile,
    AssociateDeviceWithNetworkProfile,

    -- * Request Lenses
    adwnpDeviceARN,
    adwnpNetworkProfileARN,

    -- * Destructuring the Response
    associateDeviceWithNetworkProfileResponse,
    AssociateDeviceWithNetworkProfileResponse,

    -- * Response Lenses
    adwnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateDeviceWithNetworkProfile' smart constructor.
data AssociateDeviceWithNetworkProfile = AssociateDeviceWithNetworkProfile'
  { _adwnpDeviceARN ::
      !Text,
    _adwnpNetworkProfileARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateDeviceWithNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adwnpDeviceARN' - The device ARN.
--
-- * 'adwnpNetworkProfileARN' - The ARN of the network profile to associate with a device.
associateDeviceWithNetworkProfile ::
  -- | 'adwnpDeviceARN'
  Text ->
  -- | 'adwnpNetworkProfileARN'
  Text ->
  AssociateDeviceWithNetworkProfile
associateDeviceWithNetworkProfile pDeviceARN_ pNetworkProfileARN_ =
  AssociateDeviceWithNetworkProfile'
    { _adwnpDeviceARN = pDeviceARN_,
      _adwnpNetworkProfileARN = pNetworkProfileARN_
    }

-- | The device ARN.
adwnpDeviceARN :: Lens' AssociateDeviceWithNetworkProfile Text
adwnpDeviceARN = lens _adwnpDeviceARN (\s a -> s {_adwnpDeviceARN = a})

-- | The ARN of the network profile to associate with a device.
adwnpNetworkProfileARN :: Lens' AssociateDeviceWithNetworkProfile Text
adwnpNetworkProfileARN = lens _adwnpNetworkProfileARN (\s a -> s {_adwnpNetworkProfileARN = a})

instance AWSRequest AssociateDeviceWithNetworkProfile where
  type
    Rs AssociateDeviceWithNetworkProfile =
      AssociateDeviceWithNetworkProfileResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      ( \s h x ->
          AssociateDeviceWithNetworkProfileResponse' <$> (pure (fromEnum s))
      )

instance Hashable AssociateDeviceWithNetworkProfile

instance NFData AssociateDeviceWithNetworkProfile

instance ToHeaders AssociateDeviceWithNetworkProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AlexaForBusiness.AssociateDeviceWithNetworkProfile" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateDeviceWithNetworkProfile where
  toJSON AssociateDeviceWithNetworkProfile' {..} =
    object
      ( catMaybes
          [ Just ("DeviceArn" .= _adwnpDeviceARN),
            Just ("NetworkProfileArn" .= _adwnpNetworkProfileARN)
          ]
      )

instance ToPath AssociateDeviceWithNetworkProfile where
  toPath = const "/"

instance ToQuery AssociateDeviceWithNetworkProfile where
  toQuery = const mempty

-- | /See:/ 'associateDeviceWithNetworkProfileResponse' smart constructor.
newtype AssociateDeviceWithNetworkProfileResponse = AssociateDeviceWithNetworkProfileResponse'
  { _adwnprsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateDeviceWithNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adwnprsResponseStatus' - -- | The response status code.
associateDeviceWithNetworkProfileResponse ::
  -- | 'adwnprsResponseStatus'
  Int ->
  AssociateDeviceWithNetworkProfileResponse
associateDeviceWithNetworkProfileResponse pResponseStatus_ =
  AssociateDeviceWithNetworkProfileResponse'
    { _adwnprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
adwnprsResponseStatus :: Lens' AssociateDeviceWithNetworkProfileResponse Int
adwnprsResponseStatus = lens _adwnprsResponseStatus (\s a -> s {_adwnprsResponseStatus = a})

instance NFData AssociateDeviceWithNetworkProfileResponse

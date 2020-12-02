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
-- Module      : Network.AWS.AlexaBusiness.DeleteNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.DeleteNetworkProfile
  ( -- * Creating a Request
    deleteNetworkProfile,
    DeleteNetworkProfile,

    -- * Request Lenses
    dnpNetworkProfileARN,

    -- * Destructuring the Response
    deleteNetworkProfileResponse,
    DeleteNetworkProfileResponse,

    -- * Response Lenses
    dnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNetworkProfile' smart constructor.
newtype DeleteNetworkProfile = DeleteNetworkProfile'
  { _dnpNetworkProfileARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnpNetworkProfileARN' - The ARN of the network profile associated with a device.
deleteNetworkProfile ::
  -- | 'dnpNetworkProfileARN'
  Text ->
  DeleteNetworkProfile
deleteNetworkProfile pNetworkProfileARN_ =
  DeleteNetworkProfile'
    { _dnpNetworkProfileARN =
        pNetworkProfileARN_
    }

-- | The ARN of the network profile associated with a device.
dnpNetworkProfileARN :: Lens' DeleteNetworkProfile Text
dnpNetworkProfileARN = lens _dnpNetworkProfileARN (\s a -> s {_dnpNetworkProfileARN = a})

instance AWSRequest DeleteNetworkProfile where
  type Rs DeleteNetworkProfile = DeleteNetworkProfileResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> DeleteNetworkProfileResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteNetworkProfile

instance NFData DeleteNetworkProfile

instance ToHeaders DeleteNetworkProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.DeleteNetworkProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile' {..} =
    object
      (catMaybes [Just ("NetworkProfileArn" .= _dnpNetworkProfileARN)])

instance ToPath DeleteNetworkProfile where
  toPath = const "/"

instance ToQuery DeleteNetworkProfile where
  toQuery = const mempty

-- | /See:/ 'deleteNetworkProfileResponse' smart constructor.
newtype DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { _dnprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnprsResponseStatus' - -- | The response status code.
deleteNetworkProfileResponse ::
  -- | 'dnprsResponseStatus'
  Int ->
  DeleteNetworkProfileResponse
deleteNetworkProfileResponse pResponseStatus_ =
  DeleteNetworkProfileResponse'
    { _dnprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dnprsResponseStatus :: Lens' DeleteNetworkProfileResponse Int
dnprsResponseStatus = lens _dnprsResponseStatus (\s a -> s {_dnprsResponseStatus = a})

instance NFData DeleteNetworkProfileResponse

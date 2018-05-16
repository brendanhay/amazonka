{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteNetworkProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile.
--
--
module Network.AWS.DeviceFarm.DeleteNetworkProfile
    (
    -- * Creating a Request
      deleteNetworkProfile
    , DeleteNetworkProfile
    -- * Request Lenses
    , dnpArn

    -- * Destructuring the Response
    , deleteNetworkProfileResponse
    , DeleteNetworkProfileResponse
    -- * Response Lenses
    , dnprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNetworkProfile' smart constructor.
newtype DeleteNetworkProfile = DeleteNetworkProfile'
  { _dnpArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnpArn' - The Amazon Resource Name (ARN) of the network profile you want to delete.
deleteNetworkProfile
    :: Text -- ^ 'dnpArn'
    -> DeleteNetworkProfile
deleteNetworkProfile pArn_ = DeleteNetworkProfile' {_dnpArn = pArn_}


-- | The Amazon Resource Name (ARN) of the network profile you want to delete.
dnpArn :: Lens' DeleteNetworkProfile Text
dnpArn = lens _dnpArn (\ s a -> s{_dnpArn = a})

instance AWSRequest DeleteNetworkProfile where
        type Rs DeleteNetworkProfile =
             DeleteNetworkProfileResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteNetworkProfileResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteNetworkProfile where

instance NFData DeleteNetworkProfile where

instance ToHeaders DeleteNetworkProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteNetworkProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNetworkProfile where
        toJSON DeleteNetworkProfile'{..}
          = object (catMaybes [Just ("arn" .= _dnpArn)])

instance ToPath DeleteNetworkProfile where
        toPath = const "/"

instance ToQuery DeleteNetworkProfile where
        toQuery = const mempty

-- | /See:/ 'deleteNetworkProfileResponse' smart constructor.
newtype DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { _dnprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnprsResponseStatus' - -- | The response status code.
deleteNetworkProfileResponse
    :: Int -- ^ 'dnprsResponseStatus'
    -> DeleteNetworkProfileResponse
deleteNetworkProfileResponse pResponseStatus_ =
  DeleteNetworkProfileResponse' {_dnprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dnprsResponseStatus :: Lens' DeleteNetworkProfileResponse Int
dnprsResponseStatus = lens _dnprsResponseStatus (\ s a -> s{_dnprsResponseStatus = a})

instance NFData DeleteNetworkProfileResponse where

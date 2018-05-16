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
-- Module      : Network.AWS.DeviceFarm.DeleteInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile that can be applied to one or more private device instances.
--
--
module Network.AWS.DeviceFarm.DeleteInstanceProfile
    (
    -- * Creating a Request
      deleteInstanceProfile
    , DeleteInstanceProfile
    -- * Request Lenses
    , dipArn

    -- * Destructuring the Response
    , deleteInstanceProfileResponse
    , DeleteInstanceProfileResponse
    -- * Response Lenses
    , diprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { _dipArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipArn' - The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
deleteInstanceProfile
    :: Text -- ^ 'dipArn'
    -> DeleteInstanceProfile
deleteInstanceProfile pArn_ = DeleteInstanceProfile' {_dipArn = pArn_}


-- | The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
dipArn :: Lens' DeleteInstanceProfile Text
dipArn = lens _dipArn (\ s a -> s{_dipArn = a})

instance AWSRequest DeleteInstanceProfile where
        type Rs DeleteInstanceProfile =
             DeleteInstanceProfileResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteInstanceProfileResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteInstanceProfile where

instance NFData DeleteInstanceProfile where

instance ToHeaders DeleteInstanceProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteInstanceProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInstanceProfile where
        toJSON DeleteInstanceProfile'{..}
          = object (catMaybes [Just ("arn" .= _dipArn)])

instance ToPath DeleteInstanceProfile where
        toPath = const "/"

instance ToQuery DeleteInstanceProfile where
        toQuery = const mempty

-- | /See:/ 'deleteInstanceProfileResponse' smart constructor.
newtype DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  { _diprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diprsResponseStatus' - -- | The response status code.
deleteInstanceProfileResponse
    :: Int -- ^ 'diprsResponseStatus'
    -> DeleteInstanceProfileResponse
deleteInstanceProfileResponse pResponseStatus_ =
  DeleteInstanceProfileResponse' {_diprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
diprsResponseStatus :: Lens' DeleteInstanceProfileResponse Int
diprsResponseStatus = lens _diprsResponseStatus (\ s a -> s{_diprsResponseStatus = a})

instance NFData DeleteInstanceProfileResponse where

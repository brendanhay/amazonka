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
-- Module      : Network.AWS.MediaStore.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an object lifecycle policy from a container. It takes up to 20 minutes for the change to take effect.
--
--
module Network.AWS.MediaStore.DeleteLifecyclePolicy
    (
    -- * Creating a Request
      deleteLifecyclePolicy
    , DeleteLifecyclePolicy
    -- * Request Lenses
    , dlpContainerName

    -- * Destructuring the Response
    , deleteLifecyclePolicyResponse
    , DeleteLifecyclePolicyResponse
    -- * Response Lenses
    , dlprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLifecyclePolicy' smart constructor.
newtype DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { _dlpContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlpContainerName' - The name of the container that holds the object lifecycle policy.
deleteLifecyclePolicy
    :: Text -- ^ 'dlpContainerName'
    -> DeleteLifecyclePolicy
deleteLifecyclePolicy pContainerName_ =
  DeleteLifecyclePolicy' {_dlpContainerName = pContainerName_}


-- | The name of the container that holds the object lifecycle policy.
dlpContainerName :: Lens' DeleteLifecyclePolicy Text
dlpContainerName = lens _dlpContainerName (\ s a -> s{_dlpContainerName = a})

instance AWSRequest DeleteLifecyclePolicy where
        type Rs DeleteLifecyclePolicy =
             DeleteLifecyclePolicyResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLifecyclePolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteLifecyclePolicy where

instance NFData DeleteLifecyclePolicy where

instance ToHeaders DeleteLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.DeleteLifecyclePolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLifecyclePolicy where
        toJSON DeleteLifecyclePolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _dlpContainerName)])

instance ToPath DeleteLifecyclePolicy where
        toPath = const "/"

instance ToQuery DeleteLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'deleteLifecyclePolicyResponse' smart constructor.
newtype DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { _dlprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlprsResponseStatus' - -- | The response status code.
deleteLifecyclePolicyResponse
    :: Int -- ^ 'dlprsResponseStatus'
    -> DeleteLifecyclePolicyResponse
deleteLifecyclePolicyResponse pResponseStatus_ =
  DeleteLifecyclePolicyResponse' {_dlprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlprsResponseStatus :: Lens' DeleteLifecyclePolicyResponse Int
dlprsResponseStatus = lens _dlprsResponseStatus (\ s a -> s{_dlprsResponseStatus = a})

instance NFData DeleteLifecyclePolicyResponse where

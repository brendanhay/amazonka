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
-- Module      : Network.AWS.MediaStore.DeleteContainerPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access policy that is associated with the specified container.
--
--
module Network.AWS.MediaStore.DeleteContainerPolicy
    (
    -- * Creating a Request
      deleteContainerPolicy
    , DeleteContainerPolicy
    -- * Request Lenses
    , delContainerName

    -- * Destructuring the Response
    , deleteContainerPolicyResponse
    , DeleteContainerPolicyResponse
    -- * Response Lenses
    , dcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteContainerPolicy' smart constructor.
newtype DeleteContainerPolicy = DeleteContainerPolicy'
  { _delContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteContainerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delContainerName' - The name of the container that holds the policy.
deleteContainerPolicy
    :: Text -- ^ 'delContainerName'
    -> DeleteContainerPolicy
deleteContainerPolicy pContainerName_ =
  DeleteContainerPolicy' {_delContainerName = pContainerName_}


-- | The name of the container that holds the policy.
delContainerName :: Lens' DeleteContainerPolicy Text
delContainerName = lens _delContainerName (\ s a -> s{_delContainerName = a})

instance AWSRequest DeleteContainerPolicy where
        type Rs DeleteContainerPolicy =
             DeleteContainerPolicyResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteContainerPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteContainerPolicy where

instance NFData DeleteContainerPolicy where

instance ToHeaders DeleteContainerPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.DeleteContainerPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteContainerPolicy where
        toJSON DeleteContainerPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _delContainerName)])

instance ToPath DeleteContainerPolicy where
        toPath = const "/"

instance ToQuery DeleteContainerPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteContainerPolicyResponse' smart constructor.
newtype DeleteContainerPolicyResponse = DeleteContainerPolicyResponse'
  { _dcprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteContainerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcprsResponseStatus' - -- | The response status code.
deleteContainerPolicyResponse
    :: Int -- ^ 'dcprsResponseStatus'
    -> DeleteContainerPolicyResponse
deleteContainerPolicyResponse pResponseStatus_ =
  DeleteContainerPolicyResponse' {_dcprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcprsResponseStatus :: Lens' DeleteContainerPolicyResponse Int
dcprsResponseStatus = lens _dcprsResponseStatus (\ s a -> s{_dcprsResponseStatus = a})

instance NFData DeleteContainerPolicyResponse where

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
-- Module      : Network.AWS.MediaStore.DeleteCORSPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
--
-- To use this operation, you must have permission to perform the @MediaStore:DeleteCorsPolicy@ action. The container owner has this permission by default and can grant this permission to others.
--
module Network.AWS.MediaStore.DeleteCORSPolicy
    (
    -- * Creating a Request
      deleteCORSPolicy
    , DeleteCORSPolicy
    -- * Request Lenses
    , dcpContainerName

    -- * Destructuring the Response
    , deleteCORSPolicyResponse
    , DeleteCORSPolicyResponse
    -- * Response Lenses
    , dcorsprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCORSPolicy' smart constructor.
newtype DeleteCORSPolicy = DeleteCORSPolicy'
  { _dcpContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCORSPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpContainerName' - The name of the container to remove the policy from.
deleteCORSPolicy
    :: Text -- ^ 'dcpContainerName'
    -> DeleteCORSPolicy
deleteCORSPolicy pContainerName_ =
  DeleteCORSPolicy' {_dcpContainerName = pContainerName_}


-- | The name of the container to remove the policy from.
dcpContainerName :: Lens' DeleteCORSPolicy Text
dcpContainerName = lens _dcpContainerName (\ s a -> s{_dcpContainerName = a})

instance AWSRequest DeleteCORSPolicy where
        type Rs DeleteCORSPolicy = DeleteCORSPolicyResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteCORSPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteCORSPolicy where

instance NFData DeleteCORSPolicy where

instance ToHeaders DeleteCORSPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.DeleteCorsPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCORSPolicy where
        toJSON DeleteCORSPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _dcpContainerName)])

instance ToPath DeleteCORSPolicy where
        toPath = const "/"

instance ToQuery DeleteCORSPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteCORSPolicyResponse' smart constructor.
newtype DeleteCORSPolicyResponse = DeleteCORSPolicyResponse'
  { _dcorsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCORSPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcorsprsResponseStatus' - -- | The response status code.
deleteCORSPolicyResponse
    :: Int -- ^ 'dcorsprsResponseStatus'
    -> DeleteCORSPolicyResponse
deleteCORSPolicyResponse pResponseStatus_ =
  DeleteCORSPolicyResponse' {_dcorsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcorsprsResponseStatus :: Lens' DeleteCORSPolicyResponse Int
dcorsprsResponseStatus = lens _dcorsprsResponseStatus (\ s a -> s{_dcorsprsResponseStatus = a})

instance NFData DeleteCORSPolicyResponse where

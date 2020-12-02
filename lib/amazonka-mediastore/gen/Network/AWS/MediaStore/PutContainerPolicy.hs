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
-- Module      : Network.AWS.MediaStore.PutContainerPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
--
--
-- For this release of the REST API, you can create only one policy for a container. If you enter @PutContainerPolicy@ twice, the second command modifies the existing policy.
--
module Network.AWS.MediaStore.PutContainerPolicy
    (
    -- * Creating a Request
      putContainerPolicy
    , PutContainerPolicy
    -- * Request Lenses
    , pContainerName
    , pPolicy

    -- * Destructuring the Response
    , putContainerPolicyResponse
    , PutContainerPolicyResponse
    -- * Response Lenses
    , pcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putContainerPolicy' smart constructor.
data PutContainerPolicy = PutContainerPolicy'
  { _pContainerName :: !Text
  , _pPolicy        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutContainerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pContainerName' - The name of the container.
--
-- * 'pPolicy' - The contents of the policy, which includes the following:      * One @Version@ tag     * One @Statement@ tag that contains the standard tags for the policy.
putContainerPolicy
    :: Text -- ^ 'pContainerName'
    -> Text -- ^ 'pPolicy'
    -> PutContainerPolicy
putContainerPolicy pContainerName_ pPolicy_ =
  PutContainerPolicy' {_pContainerName = pContainerName_, _pPolicy = pPolicy_}


-- | The name of the container.
pContainerName :: Lens' PutContainerPolicy Text
pContainerName = lens _pContainerName (\ s a -> s{_pContainerName = a})

-- | The contents of the policy, which includes the following:      * One @Version@ tag     * One @Statement@ tag that contains the standard tags for the policy.
pPolicy :: Lens' PutContainerPolicy Text
pPolicy = lens _pPolicy (\ s a -> s{_pPolicy = a})

instance AWSRequest PutContainerPolicy where
        type Rs PutContainerPolicy =
             PutContainerPolicyResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 PutContainerPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutContainerPolicy where

instance NFData PutContainerPolicy where

instance ToHeaders PutContainerPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.PutContainerPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutContainerPolicy where
        toJSON PutContainerPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _pContainerName),
                  Just ("Policy" .= _pPolicy)])

instance ToPath PutContainerPolicy where
        toPath = const "/"

instance ToQuery PutContainerPolicy where
        toQuery = const mempty

-- | /See:/ 'putContainerPolicyResponse' smart constructor.
newtype PutContainerPolicyResponse = PutContainerPolicyResponse'
  { _pcprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutContainerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcprsResponseStatus' - -- | The response status code.
putContainerPolicyResponse
    :: Int -- ^ 'pcprsResponseStatus'
    -> PutContainerPolicyResponse
putContainerPolicyResponse pResponseStatus_ =
  PutContainerPolicyResponse' {_pcprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
pcprsResponseStatus :: Lens' PutContainerPolicyResponse Int
pcprsResponseStatus = lens _pcprsResponseStatus (\ s a -> s{_pcprsResponseStatus = a})

instance NFData PutContainerPolicyResponse where

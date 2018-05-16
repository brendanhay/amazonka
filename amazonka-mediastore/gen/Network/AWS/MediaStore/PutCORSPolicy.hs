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
-- Module      : Network.AWS.MediaStore.PutCORSPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.
--
--
-- To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
--
module Network.AWS.MediaStore.PutCORSPolicy
    (
    -- * Creating a Request
      putCORSPolicy
    , PutCORSPolicy
    -- * Request Lenses
    , pcpContainerName
    , pcpCORSPolicy

    -- * Destructuring the Response
    , putCORSPolicyResponse
    , PutCORSPolicyResponse
    -- * Response Lenses
    , pcorsprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putCORSPolicy' smart constructor.
data PutCORSPolicy = PutCORSPolicy'
  { _pcpContainerName :: !Text
  , _pcpCORSPolicy    :: !(List1 CORSRule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutCORSPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcpContainerName' - The name of the container that you want to assign the CORS policy to.
--
-- * 'pcpCORSPolicy' - The CORS policy to apply to the container.
putCORSPolicy
    :: Text -- ^ 'pcpContainerName'
    -> NonEmpty CORSRule -- ^ 'pcpCORSPolicy'
    -> PutCORSPolicy
putCORSPolicy pContainerName_ pCORSPolicy_ =
  PutCORSPolicy'
    { _pcpContainerName = pContainerName_
    , _pcpCORSPolicy = _List1 # pCORSPolicy_
    }


-- | The name of the container that you want to assign the CORS policy to.
pcpContainerName :: Lens' PutCORSPolicy Text
pcpContainerName = lens _pcpContainerName (\ s a -> s{_pcpContainerName = a})

-- | The CORS policy to apply to the container.
pcpCORSPolicy :: Lens' PutCORSPolicy (NonEmpty CORSRule)
pcpCORSPolicy = lens _pcpCORSPolicy (\ s a -> s{_pcpCORSPolicy = a}) . _List1

instance AWSRequest PutCORSPolicy where
        type Rs PutCORSPolicy = PutCORSPolicyResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 PutCORSPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutCORSPolicy where

instance NFData PutCORSPolicy where

instance ToHeaders PutCORSPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.PutCorsPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutCORSPolicy where
        toJSON PutCORSPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _pcpContainerName),
                  Just ("CorsPolicy" .= _pcpCORSPolicy)])

instance ToPath PutCORSPolicy where
        toPath = const "/"

instance ToQuery PutCORSPolicy where
        toQuery = const mempty

-- | /See:/ 'putCORSPolicyResponse' smart constructor.
newtype PutCORSPolicyResponse = PutCORSPolicyResponse'
  { _pcorsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutCORSPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcorsprsResponseStatus' - -- | The response status code.
putCORSPolicyResponse
    :: Int -- ^ 'pcorsprsResponseStatus'
    -> PutCORSPolicyResponse
putCORSPolicyResponse pResponseStatus_ =
  PutCORSPolicyResponse' {_pcorsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
pcorsprsResponseStatus :: Lens' PutCORSPolicyResponse Int
pcorsprsResponseStatus = lens _pcorsprsResponseStatus (\ s a -> s{_pcorsprsResponseStatus = a})

instance NFData PutCORSPolicyResponse where

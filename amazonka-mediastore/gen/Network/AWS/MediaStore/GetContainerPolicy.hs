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
-- Module      : Network.AWS.MediaStore.GetContainerPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
--
--
module Network.AWS.MediaStore.GetContainerPolicy
    (
    -- * Creating a Request
      getContainerPolicy
    , GetContainerPolicy
    -- * Request Lenses
    , gContainerName

    -- * Destructuring the Response
    , getContainerPolicyResponse
    , GetContainerPolicyResponse
    -- * Response Lenses
    , gcprsResponseStatus
    , gcprsPolicy
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContainerPolicy' smart constructor.
newtype GetContainerPolicy = GetContainerPolicy'
  { _gContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContainerPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gContainerName' - The name of the container.
getContainerPolicy
    :: Text -- ^ 'gContainerName'
    -> GetContainerPolicy
getContainerPolicy pContainerName_ =
  GetContainerPolicy' {_gContainerName = pContainerName_}


-- | The name of the container.
gContainerName :: Lens' GetContainerPolicy Text
gContainerName = lens _gContainerName (\ s a -> s{_gContainerName = a})

instance AWSRequest GetContainerPolicy where
        type Rs GetContainerPolicy =
             GetContainerPolicyResponse
        request = postJSON mediaStore
        response
          = receiveJSON
              (\ s h x ->
                 GetContainerPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Policy"))

instance Hashable GetContainerPolicy where

instance NFData GetContainerPolicy where

instance ToHeaders GetContainerPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.GetContainerPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetContainerPolicy where
        toJSON GetContainerPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _gContainerName)])

instance ToPath GetContainerPolicy where
        toPath = const "/"

instance ToQuery GetContainerPolicy where
        toQuery = const mempty

-- | /See:/ 'getContainerPolicyResponse' smart constructor.
data GetContainerPolicyResponse = GetContainerPolicyResponse'
  { _gcprsResponseStatus :: !Int
  , _gcprsPolicy         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContainerPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcprsResponseStatus' - -- | The response status code.
--
-- * 'gcprsPolicy' - The contents of the access policy.
getContainerPolicyResponse
    :: Int -- ^ 'gcprsResponseStatus'
    -> Text -- ^ 'gcprsPolicy'
    -> GetContainerPolicyResponse
getContainerPolicyResponse pResponseStatus_ pPolicy_ =
  GetContainerPolicyResponse'
    {_gcprsResponseStatus = pResponseStatus_, _gcprsPolicy = pPolicy_}


-- | -- | The response status code.
gcprsResponseStatus :: Lens' GetContainerPolicyResponse Int
gcprsResponseStatus = lens _gcprsResponseStatus (\ s a -> s{_gcprsResponseStatus = a})

-- | The contents of the access policy.
gcprsPolicy :: Lens' GetContainerPolicyResponse Text
gcprsPolicy = lens _gcprsPolicy (\ s a -> s{_gcprsPolicy = a})

instance NFData GetContainerPolicyResponse where

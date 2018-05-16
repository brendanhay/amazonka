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
-- Module      : Network.AWS.MediaStore.GetCORSPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
--
-- To use this operation, you must have permission to perform the @MediaStore:GetCorsPolicy@ action. By default, the container owner has this permission and can grant it to others.
--
module Network.AWS.MediaStore.GetCORSPolicy
    (
    -- * Creating a Request
      getCORSPolicy
    , GetCORSPolicy
    -- * Request Lenses
    , gcpContainerName

    -- * Destructuring the Response
    , getCORSPolicyResponse
    , GetCORSPolicyResponse
    -- * Response Lenses
    , gcorsprsResponseStatus
    , gcorsprsCORSPolicy
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCORSPolicy' smart constructor.
newtype GetCORSPolicy = GetCORSPolicy'
  { _gcpContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCORSPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpContainerName' - The name of the container that the policy is assigned to.
getCORSPolicy
    :: Text -- ^ 'gcpContainerName'
    -> GetCORSPolicy
getCORSPolicy pContainerName_ =
  GetCORSPolicy' {_gcpContainerName = pContainerName_}


-- | The name of the container that the policy is assigned to.
gcpContainerName :: Lens' GetCORSPolicy Text
gcpContainerName = lens _gcpContainerName (\ s a -> s{_gcpContainerName = a})

instance AWSRequest GetCORSPolicy where
        type Rs GetCORSPolicy = GetCORSPolicyResponse
        request = postJSON mediaStore
        response
          = receiveJSON
              (\ s h x ->
                 GetCORSPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "CorsPolicy"))

instance Hashable GetCORSPolicy where

instance NFData GetCORSPolicy where

instance ToHeaders GetCORSPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.GetCorsPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCORSPolicy where
        toJSON GetCORSPolicy'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _gcpContainerName)])

instance ToPath GetCORSPolicy where
        toPath = const "/"

instance ToQuery GetCORSPolicy where
        toQuery = const mempty

-- | /See:/ 'getCORSPolicyResponse' smart constructor.
data GetCORSPolicyResponse = GetCORSPolicyResponse'
  { _gcorsprsResponseStatus :: !Int
  , _gcorsprsCORSPolicy     :: !(List1 CORSRule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCORSPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcorsprsResponseStatus' - -- | The response status code.
--
-- * 'gcorsprsCORSPolicy' - Undocumented member.
getCORSPolicyResponse
    :: Int -- ^ 'gcorsprsResponseStatus'
    -> NonEmpty CORSRule -- ^ 'gcorsprsCORSPolicy'
    -> GetCORSPolicyResponse
getCORSPolicyResponse pResponseStatus_ pCORSPolicy_ =
  GetCORSPolicyResponse'
    { _gcorsprsResponseStatus = pResponseStatus_
    , _gcorsprsCORSPolicy = _List1 # pCORSPolicy_
    }


-- | -- | The response status code.
gcorsprsResponseStatus :: Lens' GetCORSPolicyResponse Int
gcorsprsResponseStatus = lens _gcorsprsResponseStatus (\ s a -> s{_gcorsprsResponseStatus = a})

-- | Undocumented member.
gcorsprsCORSPolicy :: Lens' GetCORSPolicyResponse (NonEmpty CORSRule)
gcorsprsCORSPolicy = lens _gcorsprsCORSPolicy (\ s a -> s{_gcorsprsCORSPolicy = a}) . _List1

instance NFData GetCORSPolicyResponse where

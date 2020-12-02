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
-- Module      : Network.AWS.MediaPackage.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing OriginEndpoint.
module Network.AWS.MediaPackage.DeleteOriginEndpoint
    (
    -- * Creating a Request
      deleteOriginEndpoint
    , DeleteOriginEndpoint
    -- * Request Lenses
    , delId

    -- * Destructuring the Response
    , deleteOriginEndpointResponse
    , DeleteOriginEndpointResponse
    -- * Response Lenses
    , doersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOriginEndpoint' smart constructor.
newtype DeleteOriginEndpoint = DeleteOriginEndpoint'
  { _delId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delId' - The ID of the OriginEndpoint to delete.
deleteOriginEndpoint
    :: Text -- ^ 'delId'
    -> DeleteOriginEndpoint
deleteOriginEndpoint pId_ = DeleteOriginEndpoint' {_delId = pId_}


-- | The ID of the OriginEndpoint to delete.
delId :: Lens' DeleteOriginEndpoint Text
delId = lens _delId (\ s a -> s{_delId = a})

instance AWSRequest DeleteOriginEndpoint where
        type Rs DeleteOriginEndpoint =
             DeleteOriginEndpointResponse
        request = delete mediaPackage
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteOriginEndpointResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteOriginEndpoint where

instance NFData DeleteOriginEndpoint where

instance ToHeaders DeleteOriginEndpoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteOriginEndpoint where
        toPath DeleteOriginEndpoint'{..}
          = mconcat ["/origin_endpoints/", toBS _delId]

instance ToQuery DeleteOriginEndpoint where
        toQuery = const mempty

-- | /See:/ 'deleteOriginEndpointResponse' smart constructor.
newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { _doersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOriginEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doersResponseStatus' - -- | The response status code.
deleteOriginEndpointResponse
    :: Int -- ^ 'doersResponseStatus'
    -> DeleteOriginEndpointResponse
deleteOriginEndpointResponse pResponseStatus_ =
  DeleteOriginEndpointResponse' {_doersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
doersResponseStatus :: Lens' DeleteOriginEndpointResponse Int
doersResponseStatus = lens _doersResponseStatus (\ s a -> s{_doersResponseStatus = a})

instance NFData DeleteOriginEndpointResponse where

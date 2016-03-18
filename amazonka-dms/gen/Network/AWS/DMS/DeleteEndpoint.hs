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
-- Module      : Network.AWS.DMS.DeleteEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified endpoint.
--
-- All tasks associated with the endpoint must be deleted before you can
-- delete the endpoint.
--
module Network.AWS.DMS.DeleteEndpoint
    (
    -- * Creating a Request
      deleteEndpoint
    , DeleteEndpoint
    -- * Request Lenses
    , deEndpointARN

    -- * Destructuring the Response
    , deleteEndpointResponse
    , DeleteEndpointResponse
    -- * Response Lenses
    , drsEndpoint
    , drsResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
    { _deEndpointARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEndpointARN'
deleteEndpoint
    :: Text -- ^ 'deEndpointARN'
    -> DeleteEndpoint
deleteEndpoint pEndpointARN_ =
    DeleteEndpoint'
    { _deEndpointARN = pEndpointARN_
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
deEndpointARN :: Lens' DeleteEndpoint Text
deEndpointARN = lens _deEndpointARN (\ s a -> s{_deEndpointARN = a});

instance AWSRequest DeleteEndpoint where
        type Rs DeleteEndpoint = DeleteEndpointResponse
        request = postJSON dMS
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEndpointResponse' <$>
                   (x .?> "Endpoint") <*> (pure (fromEnum s)))

instance ToHeaders DeleteEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEndpoint where
        toJSON DeleteEndpoint'{..}
          = object
              (catMaybes [Just ("EndpointArn" .= _deEndpointARN)])

instance ToPath DeleteEndpoint where
        toPath = const "/"

instance ToQuery DeleteEndpoint where
        toQuery = const mempty

-- | /See:/ 'deleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
    { _drsEndpoint       :: !(Maybe Endpoint)
    , _drsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsEndpoint'
--
-- * 'drsResponseStatus'
deleteEndpointResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteEndpointResponse
deleteEndpointResponse pResponseStatus_ =
    DeleteEndpointResponse'
    { _drsEndpoint = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The endpoint that was deleted.
drsEndpoint :: Lens' DeleteEndpointResponse (Maybe Endpoint)
drsEndpoint = lens _drsEndpoint (\ s a -> s{_drsEndpoint = a});

-- | The response status code.
drsResponseStatus :: Lens' DeleteEndpointResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

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
-- Module      : Network.AWS.Glue.DeleteDevEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DevEndpoint.
--
--
module Network.AWS.Glue.DeleteDevEndpoint
    (
    -- * Creating a Request
      deleteDevEndpoint
    , DeleteDevEndpoint
    -- * Request Lenses
    , ddeEndpointName

    -- * Destructuring the Response
    , deleteDevEndpointResponse
    , DeleteDevEndpointResponse
    -- * Response Lenses
    , ddersResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDevEndpoint' smart constructor.
newtype DeleteDevEndpoint = DeleteDevEndpoint'
  { _ddeEndpointName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddeEndpointName' - The name of the DevEndpoint.
deleteDevEndpoint
    :: Text -- ^ 'ddeEndpointName'
    -> DeleteDevEndpoint
deleteDevEndpoint pEndpointName_ =
  DeleteDevEndpoint' {_ddeEndpointName = pEndpointName_}


-- | The name of the DevEndpoint.
ddeEndpointName :: Lens' DeleteDevEndpoint Text
ddeEndpointName = lens _ddeEndpointName (\ s a -> s{_ddeEndpointName = a})

instance AWSRequest DeleteDevEndpoint where
        type Rs DeleteDevEndpoint = DeleteDevEndpointResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDevEndpointResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDevEndpoint where

instance NFData DeleteDevEndpoint where

instance ToHeaders DeleteDevEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteDevEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDevEndpoint where
        toJSON DeleteDevEndpoint'{..}
          = object
              (catMaybes
                 [Just ("EndpointName" .= _ddeEndpointName)])

instance ToPath DeleteDevEndpoint where
        toPath = const "/"

instance ToQuery DeleteDevEndpoint where
        toQuery = const mempty

-- | /See:/ 'deleteDevEndpointResponse' smart constructor.
newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse'
  { _ddersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddersResponseStatus' - -- | The response status code.
deleteDevEndpointResponse
    :: Int -- ^ 'ddersResponseStatus'
    -> DeleteDevEndpointResponse
deleteDevEndpointResponse pResponseStatus_ =
  DeleteDevEndpointResponse' {_ddersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddersResponseStatus :: Lens' DeleteDevEndpointResponse Int
ddersResponseStatus = lens _ddersResponseStatus (\ s a -> s{_ddersResponseStatus = a})

instance NFData DeleteDevEndpointResponse where

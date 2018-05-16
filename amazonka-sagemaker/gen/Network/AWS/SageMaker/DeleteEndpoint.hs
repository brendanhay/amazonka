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
-- Module      : Network.AWS.SageMaker.DeleteEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint. Amazon SageMaker frees up all of the resources that were deployed when the endpoint was created.
--
--
module Network.AWS.SageMaker.DeleteEndpoint
    (
    -- * Creating a Request
      deleteEndpoint
    , DeleteEndpoint
    -- * Request Lenses
    , deEndpointName

    -- * Destructuring the Response
    , deleteEndpointResponse
    , DeleteEndpointResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
  { _deEndpointName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEndpointName' - The name of the endpoint that you want to delete.
deleteEndpoint
    :: Text -- ^ 'deEndpointName'
    -> DeleteEndpoint
deleteEndpoint pEndpointName_ =
  DeleteEndpoint' {_deEndpointName = pEndpointName_}


-- | The name of the endpoint that you want to delete.
deEndpointName :: Lens' DeleteEndpoint Text
deEndpointName = lens _deEndpointName (\ s a -> s{_deEndpointName = a})

instance AWSRequest DeleteEndpoint where
        type Rs DeleteEndpoint = DeleteEndpointResponse
        request = postJSON sageMaker
        response = receiveNull DeleteEndpointResponse'

instance Hashable DeleteEndpoint where

instance NFData DeleteEndpoint where

instance ToHeaders DeleteEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEndpoint where
        toJSON DeleteEndpoint'{..}
          = object
              (catMaybes
                 [Just ("EndpointName" .= _deEndpointName)])

instance ToPath DeleteEndpoint where
        toPath = const "/"

instance ToQuery DeleteEndpoint where
        toQuery = const mempty

-- | /See:/ 'deleteEndpointResponse' smart constructor.
data DeleteEndpointResponse =
  DeleteEndpointResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
deleteEndpointResponse
    :: DeleteEndpointResponse
deleteEndpointResponse = DeleteEndpointResponse'


instance NFData DeleteEndpointResponse where

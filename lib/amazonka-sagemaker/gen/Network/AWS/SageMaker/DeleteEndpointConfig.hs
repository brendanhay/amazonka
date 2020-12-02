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
-- Module      : Network.AWS.SageMaker.DeleteEndpointConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint configuration. The @DeleteEndpoingConfig@ API deletes only the specified configuration. It does not delete endpoints created using the configuration.
--
--
module Network.AWS.SageMaker.DeleteEndpointConfig
    (
    -- * Creating a Request
      deleteEndpointConfig
    , DeleteEndpointConfig
    -- * Request Lenses
    , dEndpointConfigName

    -- * Destructuring the Response
    , deleteEndpointConfigResponse
    , DeleteEndpointConfigResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteEndpointConfig' smart constructor.
newtype DeleteEndpointConfig = DeleteEndpointConfig'
  { _dEndpointConfigName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpointConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEndpointConfigName' - The name of the endpoint configuration that you want to delete.
deleteEndpointConfig
    :: Text -- ^ 'dEndpointConfigName'
    -> DeleteEndpointConfig
deleteEndpointConfig pEndpointConfigName_ =
  DeleteEndpointConfig' {_dEndpointConfigName = pEndpointConfigName_}


-- | The name of the endpoint configuration that you want to delete.
dEndpointConfigName :: Lens' DeleteEndpointConfig Text
dEndpointConfigName = lens _dEndpointConfigName (\ s a -> s{_dEndpointConfigName = a})

instance AWSRequest DeleteEndpointConfig where
        type Rs DeleteEndpointConfig =
             DeleteEndpointConfigResponse
        request = postJSON sageMaker
        response = receiveNull DeleteEndpointConfigResponse'

instance Hashable DeleteEndpointConfig where

instance NFData DeleteEndpointConfig where

instance ToHeaders DeleteEndpointConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteEndpointConfig" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEndpointConfig where
        toJSON DeleteEndpointConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("EndpointConfigName" .= _dEndpointConfigName)])

instance ToPath DeleteEndpointConfig where
        toPath = const "/"

instance ToQuery DeleteEndpointConfig where
        toQuery = const mempty

-- | /See:/ 'deleteEndpointConfigResponse' smart constructor.
data DeleteEndpointConfigResponse =
  DeleteEndpointConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpointConfigResponse' with the minimum fields required to make a request.
--
deleteEndpointConfigResponse
    :: DeleteEndpointConfigResponse
deleteEndpointConfigResponse = DeleteEndpointConfigResponse'


instance NFData DeleteEndpointConfigResponse where

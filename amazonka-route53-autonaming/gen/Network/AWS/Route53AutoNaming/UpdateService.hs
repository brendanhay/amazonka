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
-- Module      : Network.AWS.Route53AutoNaming.UpdateService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to perform the following operations:
--
--
--     * Add or delete @DnsRecords@ configurations
--
--     * Update the TTL setting for existing @DnsRecords@ configurations
--
--     * Add, update, or delete @HealthCheckConfig@ for a specified service
--
--
--
-- You must specify all @DnsRecords@ configurations (and, optionally, @HealthCheckConfig@ ) that you want to appear in the updated service. Any current configurations that don't appear in an @UpdateService@ request are deleted.
--
-- When you update the TTL setting for a service, Amazon Route 53 also updates the corresponding settings in all the records and health checks that were created by using the specified service.
--
module Network.AWS.Route53AutoNaming.UpdateService
    (
    -- * Creating a Request
      updateService
    , UpdateService
    -- * Request Lenses
    , usId
    , usService

    -- * Destructuring the Response
    , updateServiceResponse
    , UpdateServiceResponse
    -- * Response Lenses
    , usrsOperationId
    , usrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'updateService' smart constructor.
data UpdateService = UpdateService'
  { _usId      :: !Text
  , _usService :: !ServiceChange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usId' - The ID of the service that you want to update.
--
-- * 'usService' - A complex type that contains the new settings for the service.
updateService
    :: Text -- ^ 'usId'
    -> ServiceChange -- ^ 'usService'
    -> UpdateService
updateService pId_ pService_ =
  UpdateService' {_usId = pId_, _usService = pService_}


-- | The ID of the service that you want to update.
usId :: Lens' UpdateService Text
usId = lens _usId (\ s a -> s{_usId = a})

-- | A complex type that contains the new settings for the service.
usService :: Lens' UpdateService ServiceChange
usService = lens _usService (\ s a -> s{_usService = a})

instance AWSRequest UpdateService where
        type Rs UpdateService = UpdateServiceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' <$>
                   (x .?> "OperationId") <*> (pure (fromEnum s)))

instance Hashable UpdateService where

instance NFData UpdateService where

instance ToHeaders UpdateService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.UpdateService" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateService where
        toJSON UpdateService'{..}
          = object
              (catMaybes
                 [Just ("Id" .= _usId),
                  Just ("Service" .= _usService)])

instance ToPath UpdateService where
        toPath = const "/"

instance ToQuery UpdateService where
        toQuery = const mempty

-- | /See:/ 'updateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { _usrsOperationId    :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsOperationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateServiceResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateServiceResponse
updateServiceResponse pResponseStatus_ =
  UpdateServiceResponse'
    {_usrsOperationId = Nothing, _usrsResponseStatus = pResponseStatus_}


-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
usrsOperationId :: Lens' UpdateServiceResponse (Maybe Text)
usrsOperationId = lens _usrsOperationId (\ s a -> s{_usrsOperationId = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateServiceResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateServiceResponse where

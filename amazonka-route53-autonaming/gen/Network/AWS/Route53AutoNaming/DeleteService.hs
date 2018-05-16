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
-- Module      : Network.AWS.Route53AutoNaming.DeleteService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service. If the service still contains one or more registered instances, the request fails.
--
--
module Network.AWS.Route53AutoNaming.DeleteService
    (
    -- * Creating a Request
      deleteService
    , DeleteService
    -- * Request Lenses
    , dsId

    -- * Destructuring the Response
    , deleteServiceResponse
    , DeleteServiceResponse
    -- * Response Lenses
    , dsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'deleteService' smart constructor.
newtype DeleteService = DeleteService'
  { _dsId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId' - The ID of the service that you want to delete.
deleteService
    :: Text -- ^ 'dsId'
    -> DeleteService
deleteService pId_ = DeleteService' {_dsId = pId_}


-- | The ID of the service that you want to delete.
dsId :: Lens' DeleteService Text
dsId = lens _dsId (\ s a -> s{_dsId = a})

instance AWSRequest DeleteService where
        type Rs DeleteService = DeleteServiceResponse
        request = postJSON route53AutoNaming
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteServiceResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteService where

instance NFData DeleteService where

instance ToHeaders DeleteService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.DeleteService" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteService where
        toJSON DeleteService'{..}
          = object (catMaybes [Just ("Id" .= _dsId)])

instance ToPath DeleteService where
        toPath = const "/"

instance ToQuery DeleteService where
        toQuery = const mempty

-- | /See:/ 'deleteServiceResponse' smart constructor.
newtype DeleteServiceResponse = DeleteServiceResponse'
  { _dsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteServiceResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteServiceResponse
deleteServiceResponse pResponseStatus_ =
  DeleteServiceResponse' {_dsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteServiceResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteServiceResponse where

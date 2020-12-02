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
-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a health check.
--
--
-- /Important:/ Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html Replacing and Deleting Health Checks> in the /Amazon Route 53 Developer Guide/ .
--
module Network.AWS.Route53.DeleteHealthCheck
    (
    -- * Creating a Request
      deleteHealthCheck
    , DeleteHealthCheck
    -- * Request Lenses
    , dhcHealthCheckId

    -- * Destructuring the Response
    , deleteHealthCheckResponse
    , DeleteHealthCheckResponse
    -- * Response Lenses
    , dhcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | This action deletes a health check.
--
--
--
-- /See:/ 'deleteHealthCheck' smart constructor.
newtype DeleteHealthCheck = DeleteHealthCheck'
  { _dhcHealthCheckId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcHealthCheckId' - The ID of the health check that you want to delete.
deleteHealthCheck
    :: Text -- ^ 'dhcHealthCheckId'
    -> DeleteHealthCheck
deleteHealthCheck pHealthCheckId_ =
  DeleteHealthCheck' {_dhcHealthCheckId = pHealthCheckId_}


-- | The ID of the health check that you want to delete.
dhcHealthCheckId :: Lens' DeleteHealthCheck Text
dhcHealthCheckId = lens _dhcHealthCheckId (\ s a -> s{_dhcHealthCheckId = a})

instance AWSRequest DeleteHealthCheck where
        type Rs DeleteHealthCheck = DeleteHealthCheckResponse
        request = delete route53
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteHealthCheckResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteHealthCheck where

instance NFData DeleteHealthCheck where

instance ToHeaders DeleteHealthCheck where
        toHeaders = const mempty

instance ToPath DeleteHealthCheck where
        toPath DeleteHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _dhcHealthCheckId]

instance ToQuery DeleteHealthCheck where
        toQuery = const mempty

-- | An empty element.
--
--
--
-- /See:/ 'deleteHealthCheckResponse' smart constructor.
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
  { _dhcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcrsResponseStatus' - -- | The response status code.
deleteHealthCheckResponse
    :: Int -- ^ 'dhcrsResponseStatus'
    -> DeleteHealthCheckResponse
deleteHealthCheckResponse pResponseStatus_ =
  DeleteHealthCheckResponse' {_dhcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dhcrsResponseStatus :: Lens' DeleteHealthCheckResponse Int
dhcrsResponseStatus = lens _dhcrsResponseStatus (\ s a -> s{_dhcrsResponseStatus = a})

instance NFData DeleteHealthCheckResponse where

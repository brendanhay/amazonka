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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a health check. To delete a health check, send a
-- 'DELETE' request to the
-- '\/Route 53 API version\/healthcheck\/health check ID' resource.
--
-- You can delete a health check only if there are no resource record sets
-- associated with this health check. If resource record sets are
-- associated with this health check, you must disassociate them before you
-- can delete your health check. If you try to delete a health check that
-- is associated with resource record sets, Amazon Route 53 will deny your
-- request with a 'HealthCheckInUse' error. For information about
-- disassociating the records from your health check, see
-- < ChangeResourceRecordSets>.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type containing the request information for delete health
-- check.
--
-- /See:/ 'deleteHealthCheck' smart constructor.
newtype DeleteHealthCheck = DeleteHealthCheck'
    { _dhcHealthCheckId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcHealthCheckId'
deleteHealthCheck
    :: Text -- ^ 'dhcHealthCheckId'
    -> DeleteHealthCheck
deleteHealthCheck pHealthCheckId_ =
    DeleteHealthCheck'
    { _dhcHealthCheckId = pHealthCheckId_
    }

-- | The ID of the health check to delete.
dhcHealthCheckId :: Lens' DeleteHealthCheck Text
dhcHealthCheckId = lens _dhcHealthCheckId (\ s a -> s{_dhcHealthCheckId = a});

instance AWSRequest DeleteHealthCheck where
        type Rs DeleteHealthCheck = DeleteHealthCheckResponse
        request = delete route53
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteHealthCheckResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteHealthCheck

instance ToHeaders DeleteHealthCheck where
        toHeaders = const mempty

instance ToPath DeleteHealthCheck where
        toPath DeleteHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _dhcHealthCheckId]

instance ToQuery DeleteHealthCheck where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteHealthCheckResponse' smart constructor.
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
    { _dhcrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcrsResponseStatus'
deleteHealthCheckResponse
    :: Int -- ^ 'dhcrsResponseStatus'
    -> DeleteHealthCheckResponse
deleteHealthCheckResponse pResponseStatus_ =
    DeleteHealthCheckResponse'
    { _dhcrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
dhcrsResponseStatus :: Lens' DeleteHealthCheckResponse Int
dhcrsResponseStatus = lens _dhcrsResponseStatus (\ s a -> s{_dhcrsResponseStatus = a});

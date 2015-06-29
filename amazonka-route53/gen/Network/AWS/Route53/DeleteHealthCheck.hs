{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action deletes a health check. To delete a health check, send a
-- @DELETE@ request to the @2013-04-01\/healthcheck\/health check ID@
-- resource.
--
-- You can delete a health check only if there are no resource record sets
-- associated with this health check. If resource record sets are
-- associated with this health check, you must disassociate them before you
-- can delete your health check. If you try to delete a health check that
-- is associated with resource record sets, Route 53 will deny your request
-- with a @HealthCheckInUse@ error. For information about disassociating
-- the records from your health check, see ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHealthCheck.html>
module Network.AWS.Route53.DeleteHealthCheck
    (
    -- * Request
      DeleteHealthCheck
    -- ** Request constructor
    , deleteHealthCheck
    -- ** Request lenses
    , dhcHealthCheckId

    -- * Response
    , DeleteHealthCheckResponse
    -- ** Response constructor
    , deleteHealthCheckResponse
    -- ** Response lenses
    , dhcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type containing the request information for delete health
-- check.
--
-- /See:/ 'deleteHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcHealthCheckId'
newtype DeleteHealthCheck = DeleteHealthCheck'
    { _dhcHealthCheckId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteHealthCheck' smart constructor.
deleteHealthCheck :: Text -> DeleteHealthCheck
deleteHealthCheck pHealthCheckId =
    DeleteHealthCheck'
    { _dhcHealthCheckId = pHealthCheckId
    }

-- | The ID of the health check to delete.
dhcHealthCheckId :: Lens' DeleteHealthCheck Text
dhcHealthCheckId = lens _dhcHealthCheckId (\ s a -> s{_dhcHealthCheckId = a});

instance AWSRequest DeleteHealthCheck where
        type Sv DeleteHealthCheck = Route53
        type Rs DeleteHealthCheck = DeleteHealthCheckResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 DeleteHealthCheckResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteHealthCheck where
        toHeaders = const mempty

instance ToPath DeleteHealthCheck where
        toPath DeleteHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toText _dhcHealthCheckId]

instance ToQuery DeleteHealthCheck where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcrStatus'
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
    { _dhcrStatus :: Int
    } deriving (Eq,Read,Show)

-- | 'DeleteHealthCheckResponse' smart constructor.
deleteHealthCheckResponse :: Int -> DeleteHealthCheckResponse
deleteHealthCheckResponse pStatus =
    DeleteHealthCheckResponse'
    { _dhcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dhcrStatus :: Lens' DeleteHealthCheckResponse Int
dhcrStatus = lens _dhcrStatus (\ s a -> s{_dhcrStatus = a});

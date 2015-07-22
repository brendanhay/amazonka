{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a health check. To delete a health check, send a
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
    , dhcrqHealthCheckId

    -- * Response
    , DeleteHealthCheckResponse
    -- ** Response constructor
    , deleteHealthCheckResponse
    -- ** Response lenses
    , dhcrsStatus
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
-- * 'dhcrqHealthCheckId'
newtype DeleteHealthCheck = DeleteHealthCheck'
    { _dhcrqHealthCheckId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHealthCheck' smart constructor.
deleteHealthCheck :: Text -> DeleteHealthCheck
deleteHealthCheck pHealthCheckId =
    DeleteHealthCheck'
    { _dhcrqHealthCheckId = pHealthCheckId
    }

-- | The ID of the health check to delete.
dhcrqHealthCheckId :: Lens' DeleteHealthCheck Text
dhcrqHealthCheckId = lens _dhcrqHealthCheckId (\ s a -> s{_dhcrqHealthCheckId = a});

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
               toText _dhcrqHealthCheckId]

instance ToQuery DeleteHealthCheck where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcrsStatus'
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
    { _dhcrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHealthCheckResponse' smart constructor.
deleteHealthCheckResponse :: Int -> DeleteHealthCheckResponse
deleteHealthCheckResponse pStatus =
    DeleteHealthCheckResponse'
    { _dhcrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dhcrsStatus :: Lens' DeleteHealthCheckResponse Int
dhcrsStatus = lens _dhcrsStatus (\ s a -> s{_dhcrsStatus = a});

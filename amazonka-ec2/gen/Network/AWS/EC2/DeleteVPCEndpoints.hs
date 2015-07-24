{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPCEndpoints
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more specified VPC endpoints. Deleting the endpoint also
-- deletes the endpoint routes in the route tables that were associated
-- with the endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPCEndpoints.html>
module Network.AWS.EC2.DeleteVPCEndpoints
    (
    -- * Request
      DeleteVPCEndpoints
    -- ** Request constructor
    , deleteVPCEndpoints
    -- ** Request lenses
    , dveDryRun
    , dveVPCEndpointIds

    -- * Response
    , DeleteVPCEndpointsResponse
    -- ** Response constructor
    , deleteVPCEndpointsResponse
    -- ** Response lenses
    , dversUnsuccessful
    , dversStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPCEndpoints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dveDryRun'
--
-- * 'dveVPCEndpointIds'
data DeleteVPCEndpoints = DeleteVPCEndpoints'
    { _dveDryRun         :: !(Maybe Bool)
    , _dveVPCEndpointIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPCEndpoints' smart constructor.
deleteVPCEndpoints :: DeleteVPCEndpoints
deleteVPCEndpoints =
    DeleteVPCEndpoints'
    { _dveDryRun = Nothing
    , _dveVPCEndpointIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dveDryRun :: Lens' DeleteVPCEndpoints (Maybe Bool)
dveDryRun = lens _dveDryRun (\ s a -> s{_dveDryRun = a});

-- | One or more endpoint IDs.
dveVPCEndpointIds :: Lens' DeleteVPCEndpoints [Text]
dveVPCEndpointIds = lens _dveVPCEndpointIds (\ s a -> s{_dveVPCEndpointIds = a});

instance AWSRequest DeleteVPCEndpoints where
        type Sv DeleteVPCEndpoints = EC2
        type Rs DeleteVPCEndpoints =
             DeleteVPCEndpointsResponse
        request = post "DeleteVPCEndpoints"
        response
          = receiveXML
              (\ s h x ->
                 DeleteVPCEndpointsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DeleteVPCEndpoints where
        toHeaders = const mempty

instance ToPath DeleteVPCEndpoints where
        toPath = const "/"

instance ToQuery DeleteVPCEndpoints where
        toQuery DeleteVPCEndpoints'{..}
          = mconcat
              ["Action" =: ("DeleteVPCEndpoints" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dveDryRun,
               toQueryList "item" _dveVPCEndpointIds]

-- | /See:/ 'deleteVPCEndpointsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dversUnsuccessful'
--
-- * 'dversStatus'
data DeleteVPCEndpointsResponse = DeleteVPCEndpointsResponse'
    { _dversUnsuccessful :: !(Maybe [UnsuccessfulItem])
    , _dversStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPCEndpointsResponse' smart constructor.
deleteVPCEndpointsResponse :: Int -> DeleteVPCEndpointsResponse
deleteVPCEndpointsResponse pStatus_ =
    DeleteVPCEndpointsResponse'
    { _dversUnsuccessful = Nothing
    , _dversStatus = pStatus_
    }

-- | Information about the endpoints that were not successfully deleted.
dversUnsuccessful :: Lens' DeleteVPCEndpointsResponse [UnsuccessfulItem]
dversUnsuccessful = lens _dversUnsuccessful (\ s a -> s{_dversUnsuccessful = a}) . _Default;

-- | FIXME: Undocumented member.
dversStatus :: Lens' DeleteVPCEndpointsResponse Int
dversStatus = lens _dversStatus (\ s a -> s{_dversStatus = a});

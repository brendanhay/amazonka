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
-- Module      : Network.AWS.EC2.DeleteFleets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EC2 Fleet.
--
--
-- After you delete an EC2 Fleet, it launches no new instances. You must specify whether an EC2 Fleet should also terminate its instances. If you terminate the instances, the EC2 Fleet enters the @deleted_terminating@ state. Otherwise, the EC2 Fleet enters the @deleted_running@ state, and the instances continue to run until they are interrupted or you terminate them manually.
--
module Network.AWS.EC2.DeleteFleets
    (
    -- * Creating a Request
      deleteFleets
    , DeleteFleets
    -- * Request Lenses
    , dfDryRun
    , dfFleetIds
    , dfTerminateInstances

    -- * Destructuring the Response
    , deleteFleetsResponse
    , DeleteFleetsResponse
    -- * Response Lenses
    , dfrsSuccessfulFleetDeletions
    , dfrsUnsuccessfulFleetDeletions
    , dfrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFleets' smart constructor.
data DeleteFleets = DeleteFleets'
  { _dfDryRun             :: !(Maybe Bool)
  , _dfFleetIds           :: ![Text]
  , _dfTerminateInstances :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfFleetIds' - The IDs of the EC2 Fleets.
--
-- * 'dfTerminateInstances' - Indicates whether to terminate instances for an EC2 Fleet if it is deleted successfully.
deleteFleets
    :: Bool -- ^ 'dfTerminateInstances'
    -> DeleteFleets
deleteFleets pTerminateInstances_ =
  DeleteFleets'
    { _dfDryRun = Nothing
    , _dfFleetIds = mempty
    , _dfTerminateInstances = pTerminateInstances_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfDryRun :: Lens' DeleteFleets (Maybe Bool)
dfDryRun = lens _dfDryRun (\ s a -> s{_dfDryRun = a})

-- | The IDs of the EC2 Fleets.
dfFleetIds :: Lens' DeleteFleets [Text]
dfFleetIds = lens _dfFleetIds (\ s a -> s{_dfFleetIds = a}) . _Coerce

-- | Indicates whether to terminate instances for an EC2 Fleet if it is deleted successfully.
dfTerminateInstances :: Lens' DeleteFleets Bool
dfTerminateInstances = lens _dfTerminateInstances (\ s a -> s{_dfTerminateInstances = a})

instance AWSRequest DeleteFleets where
        type Rs DeleteFleets = DeleteFleetsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteFleetsResponse' <$>
                   (x .@? "successfulFleetDeletionSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "unsuccessfulFleetDeletionSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DeleteFleets where

instance NFData DeleteFleets where

instance ToHeaders DeleteFleets where
        toHeaders = const mempty

instance ToPath DeleteFleets where
        toPath = const "/"

instance ToQuery DeleteFleets where
        toQuery DeleteFleets'{..}
          = mconcat
              ["Action" =: ("DeleteFleets" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dfDryRun,
               toQueryList "FleetId" _dfFleetIds,
               "TerminateInstances" =: _dfTerminateInstances]

-- | /See:/ 'deleteFleetsResponse' smart constructor.
data DeleteFleetsResponse = DeleteFleetsResponse'
  { _dfrsSuccessfulFleetDeletions   :: !(Maybe [DeleteFleetSuccessItem])
  , _dfrsUnsuccessfulFleetDeletions :: !(Maybe [DeleteFleetErrorItem])
  , _dfrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsSuccessfulFleetDeletions' - Information about the EC2 Fleets that are successfully deleted.
--
-- * 'dfrsUnsuccessfulFleetDeletions' - Information about the EC2 Fleets that are not successfully deleted.
--
-- * 'dfrsResponseStatus' - -- | The response status code.
deleteFleetsResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DeleteFleetsResponse
deleteFleetsResponse pResponseStatus_ =
  DeleteFleetsResponse'
    { _dfrsSuccessfulFleetDeletions = Nothing
    , _dfrsUnsuccessfulFleetDeletions = Nothing
    , _dfrsResponseStatus = pResponseStatus_
    }


-- | Information about the EC2 Fleets that are successfully deleted.
dfrsSuccessfulFleetDeletions :: Lens' DeleteFleetsResponse [DeleteFleetSuccessItem]
dfrsSuccessfulFleetDeletions = lens _dfrsSuccessfulFleetDeletions (\ s a -> s{_dfrsSuccessfulFleetDeletions = a}) . _Default . _Coerce

-- | Information about the EC2 Fleets that are not successfully deleted.
dfrsUnsuccessfulFleetDeletions :: Lens' DeleteFleetsResponse [DeleteFleetErrorItem]
dfrsUnsuccessfulFleetDeletions = lens _dfrsUnsuccessfulFleetDeletions (\ s a -> s{_dfrsUnsuccessfulFleetDeletions = a}) . _Default . _Coerce

-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFleetsResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DeleteFleetsResponse where

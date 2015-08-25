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
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Spot fleet requests.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotFleetRequests.html AWS API Reference> for CancelSpotFleetRequests.
module Network.AWS.EC2.CancelSpotFleetRequests
    (
    -- * Creating a Request
      cancelSpotFleetRequests
    , CancelSpotFleetRequests
    -- * Request Lenses
    , csfrDryRun
    , csfrSpotFleetRequestIds
    , csfrTerminateInstances

    -- * Destructuring the Response
    , cancelSpotFleetRequestsResponse
    , CancelSpotFleetRequestsResponse
    -- * Response Lenses
    , csfrrsSuccessfulFleetRequests
    , csfrrsUnsuccessfulFleetRequests
    , csfrrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CancelSpotFleetRequests.
--
-- /See:/ 'cancelSpotFleetRequests' smart constructor.
data CancelSpotFleetRequests = CancelSpotFleetRequests'
    { _csfrDryRun              :: !(Maybe Bool)
    , _csfrSpotFleetRequestIds :: ![Text]
    , _csfrTerminateInstances  :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelSpotFleetRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfrDryRun'
--
-- * 'csfrSpotFleetRequestIds'
--
-- * 'csfrTerminateInstances'
cancelSpotFleetRequests
    :: Bool -- ^ 'csfrTerminateInstances'
    -> CancelSpotFleetRequests
cancelSpotFleetRequests pTerminateInstances_ =
    CancelSpotFleetRequests'
    { _csfrDryRun = Nothing
    , _csfrSpotFleetRequestIds = mempty
    , _csfrTerminateInstances = pTerminateInstances_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
csfrDryRun :: Lens' CancelSpotFleetRequests (Maybe Bool)
csfrDryRun = lens _csfrDryRun (\ s a -> s{_csfrDryRun = a});

-- | The IDs of the Spot fleet requests.
csfrSpotFleetRequestIds :: Lens' CancelSpotFleetRequests [Text]
csfrSpotFleetRequestIds = lens _csfrSpotFleetRequestIds (\ s a -> s{_csfrSpotFleetRequestIds = a}) . _Coerce;

-- | Indicates whether to terminate instances for a Spot fleet request if it
-- is canceled successfully.
csfrTerminateInstances :: Lens' CancelSpotFleetRequests Bool
csfrTerminateInstances = lens _csfrTerminateInstances (\ s a -> s{_csfrTerminateInstances = a});

instance AWSRequest CancelSpotFleetRequests where
        type Rs CancelSpotFleetRequests =
             CancelSpotFleetRequestsResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 CancelSpotFleetRequestsResponse' <$>
                   (x .@? "successfulFleetRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "unsuccessfulFleetRequestSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders CancelSpotFleetRequests where
        toHeaders = const mempty

instance ToPath CancelSpotFleetRequests where
        toPath = const "/"

instance ToQuery CancelSpotFleetRequests where
        toQuery CancelSpotFleetRequests'{..}
          = mconcat
              ["Action" =:
                 ("CancelSpotFleetRequests" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _csfrDryRun,
               toQueryList "SpotFleetRequestId"
                 _csfrSpotFleetRequestIds,
               "TerminateInstances" =: _csfrTerminateInstances]

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'cancelSpotFleetRequestsResponse' smart constructor.
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
    { _csfrrsSuccessfulFleetRequests   :: !(Maybe [CancelSpotFleetRequestsSuccessItem])
    , _csfrrsUnsuccessfulFleetRequests :: !(Maybe [CancelSpotFleetRequestsErrorItem])
    , _csfrrsStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelSpotFleetRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfrrsSuccessfulFleetRequests'
--
-- * 'csfrrsUnsuccessfulFleetRequests'
--
-- * 'csfrrsStatus'
cancelSpotFleetRequestsResponse
    :: Int -- ^ 'csfrrsStatus'
    -> CancelSpotFleetRequestsResponse
cancelSpotFleetRequestsResponse pStatus_ =
    CancelSpotFleetRequestsResponse'
    { _csfrrsSuccessfulFleetRequests = Nothing
    , _csfrrsUnsuccessfulFleetRequests = Nothing
    , _csfrrsStatus = pStatus_
    }

-- | Information about the Spot fleet requests that are successfully
-- canceled.
csfrrsSuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsSuccessItem]
csfrrsSuccessfulFleetRequests = lens _csfrrsSuccessfulFleetRequests (\ s a -> s{_csfrrsSuccessfulFleetRequests = a}) . _Default . _Coerce;

-- | Information about the Spot fleet requests that are not successfully
-- canceled.
csfrrsUnsuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsErrorItem]
csfrrsUnsuccessfulFleetRequests = lens _csfrrsUnsuccessfulFleetRequests (\ s a -> s{_csfrrsUnsuccessfulFleetRequests = a}) . _Default . _Coerce;

-- | The response status code.
csfrrsStatus :: Lens' CancelSpotFleetRequestsResponse Int
csfrrsStatus = lens _csfrrsStatus (\ s a -> s{_csfrrsStatus = a});

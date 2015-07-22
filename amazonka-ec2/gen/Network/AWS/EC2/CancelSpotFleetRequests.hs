{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Spot fleet requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotFleetRequests.html>
module Network.AWS.EC2.CancelSpotFleetRequests
    (
    -- * Request
      CancelSpotFleetRequests
    -- ** Request constructor
    , cancelSpotFleetRequests
    -- ** Request lenses
    , csfrrqDryRun
    , csfrrqSpotFleetRequestIds
    , csfrrqTerminateInstances

    -- * Response
    , CancelSpotFleetRequestsResponse
    -- ** Response constructor
    , cancelSpotFleetRequestsResponse
    -- ** Response lenses
    , csfrrsSuccessfulFleetRequests
    , csfrrsUnsuccessfulFleetRequests
    , csfrrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CancelSpotFleetRequests.
--
-- /See:/ 'cancelSpotFleetRequests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrrqDryRun'
--
-- * 'csfrrqSpotFleetRequestIds'
--
-- * 'csfrrqTerminateInstances'
data CancelSpotFleetRequests = CancelSpotFleetRequests'
    { _csfrrqDryRun              :: !(Maybe Bool)
    , _csfrrqSpotFleetRequestIds :: ![Text]
    , _csfrrqTerminateInstances  :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotFleetRequests' smart constructor.
cancelSpotFleetRequests :: Bool -> CancelSpotFleetRequests
cancelSpotFleetRequests pTerminateInstances =
    CancelSpotFleetRequests'
    { _csfrrqDryRun = Nothing
    , _csfrrqSpotFleetRequestIds = mempty
    , _csfrrqTerminateInstances = pTerminateInstances
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csfrrqDryRun :: Lens' CancelSpotFleetRequests (Maybe Bool)
csfrrqDryRun = lens _csfrrqDryRun (\ s a -> s{_csfrrqDryRun = a});

-- | The IDs of the Spot fleet requests.
csfrrqSpotFleetRequestIds :: Lens' CancelSpotFleetRequests [Text]
csfrrqSpotFleetRequestIds = lens _csfrrqSpotFleetRequestIds (\ s a -> s{_csfrrqSpotFleetRequestIds = a});

-- | Indicates whether to terminate instances for a Spot fleet request if it
-- is canceled successfully.
csfrrqTerminateInstances :: Lens' CancelSpotFleetRequests Bool
csfrrqTerminateInstances = lens _csfrrqTerminateInstances (\ s a -> s{_csfrrqTerminateInstances = a});

instance AWSRequest CancelSpotFleetRequests where
        type Sv CancelSpotFleetRequests = EC2
        type Rs CancelSpotFleetRequests =
             CancelSpotFleetRequestsResponse
        request = post
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
               "DryRun" =: _csfrrqDryRun,
               toQueryList "item" _csfrrqSpotFleetRequestIds,
               "TerminateInstances" =: _csfrrqTerminateInstances]

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'cancelSpotFleetRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrrsSuccessfulFleetRequests'
--
-- * 'csfrrsUnsuccessfulFleetRequests'
--
-- * 'csfrrsStatus'
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
    { _csfrrsSuccessfulFleetRequests   :: !(Maybe [CancelSpotFleetRequestsSuccessItem])
    , _csfrrsUnsuccessfulFleetRequests :: !(Maybe [CancelSpotFleetRequestsErrorItem])
    , _csfrrsStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotFleetRequestsResponse' smart constructor.
cancelSpotFleetRequestsResponse :: Int -> CancelSpotFleetRequestsResponse
cancelSpotFleetRequestsResponse pStatus =
    CancelSpotFleetRequestsResponse'
    { _csfrrsSuccessfulFleetRequests = Nothing
    , _csfrrsUnsuccessfulFleetRequests = Nothing
    , _csfrrsStatus = pStatus
    }

-- | Information about the Spot fleet requests that are successfully
-- canceled.
csfrrsSuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsSuccessItem]
csfrrsSuccessfulFleetRequests = lens _csfrrsSuccessfulFleetRequests (\ s a -> s{_csfrrsSuccessfulFleetRequests = a}) . _Default;

-- | Information about the Spot fleet requests that are not successfully
-- canceled.
csfrrsUnsuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsErrorItem]
csfrrsUnsuccessfulFleetRequests = lens _csfrrsUnsuccessfulFleetRequests (\ s a -> s{_csfrrsUnsuccessfulFleetRequests = a}) . _Default;

-- | FIXME: Undocumented member.
csfrrsStatus :: Lens' CancelSpotFleetRequestsResponse Int
csfrrsStatus = lens _csfrrsStatus (\ s a -> s{_csfrrsStatus = a});

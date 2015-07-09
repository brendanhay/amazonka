{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Cancels the specified Spot fleet requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotFleetRequests.html>
module Network.AWS.EC2.CancelSpotFleetRequests
    (
    -- * Request
      CancelSpotFleetRequests
    -- ** Request constructor
    , cancelSpotFleetRequests
    -- ** Request lenses
    , csfrDryRun
    , csfrSpotFleetRequestIds
    , csfrTerminateInstances

    -- * Response
    , CancelSpotFleetRequestsResponse
    -- ** Response constructor
    , cancelSpotFleetRequestsResponse
    -- ** Response lenses
    , csfrrSuccessfulFleetRequests
    , csfrrUnsuccessfulFleetRequests
    , csfrrStatus
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
-- * 'csfrDryRun'
--
-- * 'csfrSpotFleetRequestIds'
--
-- * 'csfrTerminateInstances'
data CancelSpotFleetRequests = CancelSpotFleetRequests'
    { _csfrDryRun              :: !(Maybe Bool)
    , _csfrSpotFleetRequestIds :: ![Text]
    , _csfrTerminateInstances  :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotFleetRequests' smart constructor.
cancelSpotFleetRequests :: Bool -> CancelSpotFleetRequests
cancelSpotFleetRequests pTerminateInstances =
    CancelSpotFleetRequests'
    { _csfrDryRun = Nothing
    , _csfrSpotFleetRequestIds = mempty
    , _csfrTerminateInstances = pTerminateInstances
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csfrDryRun :: Lens' CancelSpotFleetRequests (Maybe Bool)
csfrDryRun = lens _csfrDryRun (\ s a -> s{_csfrDryRun = a});

-- | The IDs of the Spot fleet requests.
csfrSpotFleetRequestIds :: Lens' CancelSpotFleetRequests [Text]
csfrSpotFleetRequestIds = lens _csfrSpotFleetRequestIds (\ s a -> s{_csfrSpotFleetRequestIds = a});

-- | Indicates whether to terminate instances for a Spot fleet request if it
-- is canceled successfully.
csfrTerminateInstances :: Lens' CancelSpotFleetRequests Bool
csfrTerminateInstances = lens _csfrTerminateInstances (\ s a -> s{_csfrTerminateInstances = a});

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
               "DryRun" =: _csfrDryRun,
               toQueryList "item" _csfrSpotFleetRequestIds,
               "TerminateInstances" =: _csfrTerminateInstances]

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'cancelSpotFleetRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrrSuccessfulFleetRequests'
--
-- * 'csfrrUnsuccessfulFleetRequests'
--
-- * 'csfrrStatus'
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
    { _csfrrSuccessfulFleetRequests   :: !(Maybe [CancelSpotFleetRequestsSuccessItem])
    , _csfrrUnsuccessfulFleetRequests :: !(Maybe [CancelSpotFleetRequestsErrorItem])
    , _csfrrStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotFleetRequestsResponse' smart constructor.
cancelSpotFleetRequestsResponse :: Int -> CancelSpotFleetRequestsResponse
cancelSpotFleetRequestsResponse pStatus =
    CancelSpotFleetRequestsResponse'
    { _csfrrSuccessfulFleetRequests = Nothing
    , _csfrrUnsuccessfulFleetRequests = Nothing
    , _csfrrStatus = pStatus
    }

-- | Information about the Spot fleet requests that are successfully
-- canceled.
csfrrSuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsSuccessItem]
csfrrSuccessfulFleetRequests = lens _csfrrSuccessfulFleetRequests (\ s a -> s{_csfrrSuccessfulFleetRequests = a}) . _Default;

-- | Information about the Spot fleet requests that are not successfully
-- canceled.
csfrrUnsuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsErrorItem]
csfrrUnsuccessfulFleetRequests = lens _csfrrUnsuccessfulFleetRequests (\ s a -> s{_csfrrUnsuccessfulFleetRequests = a}) . _Default;

-- | FIXME: Undocumented member.
csfrrStatus :: Lens' CancelSpotFleetRequestsResponse Int
csfrrStatus = lens _csfrrStatus (\ s a -> s{_csfrrStatus = a});

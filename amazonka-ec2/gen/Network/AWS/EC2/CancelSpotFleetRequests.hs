{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'cancelSpotFleetRequests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrDryRun'
--
-- * 'csfrSpotFleetRequestIds'
--
-- * 'csfrTerminateInstances'
data CancelSpotFleetRequests = CancelSpotFleetRequests'{_csfrDryRun :: Maybe Bool, _csfrSpotFleetRequestIds :: [Text], _csfrTerminateInstances :: Bool} deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequests' smart constructor.
cancelSpotFleetRequests :: [Text] -> Bool -> CancelSpotFleetRequests
cancelSpotFleetRequests pSpotFleetRequestIds pTerminateInstances = CancelSpotFleetRequests'{_csfrDryRun = Nothing, _csfrSpotFleetRequestIds = pSpotFleetRequestIds, _csfrTerminateInstances = pTerminateInstances};

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
                   parseXMLList "item" x <*> parseXMLList "item" x)

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
               "item" =: _csfrSpotFleetRequestIds,
               "TerminateInstances" =: _csfrTerminateInstances]

-- | /See:/ 'cancelSpotFleetRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrrSuccessfulFleetRequests'
--
-- * 'csfrrUnsuccessfulFleetRequests'
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'{_csfrrSuccessfulFleetRequests :: [CancelSpotFleetRequestsSuccessItem], _csfrrUnsuccessfulFleetRequests :: [CancelSpotFleetRequestsErrorItem]} deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsResponse' smart constructor.
cancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse
cancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'{_csfrrSuccessfulFleetRequests = mempty, _csfrrUnsuccessfulFleetRequests = mempty};

-- | Information about the Spot fleet requests that are successfully
-- canceled.
csfrrSuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsSuccessItem]
csfrrSuccessfulFleetRequests = lens _csfrrSuccessfulFleetRequests (\ s a -> s{_csfrrSuccessfulFleetRequests = a});

-- | Information about the Spot fleet requests that are not successfully
-- canceled.
csfrrUnsuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsErrorItem]
csfrrUnsuccessfulFleetRequests = lens _csfrrUnsuccessfulFleetRequests (\ s a -> s{_csfrrUnsuccessfulFleetRequests = a});

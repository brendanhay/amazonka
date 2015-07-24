{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot Instance requests. Spot Instances are instances
-- that Amazon EC2 starts on your behalf when the bid price that you
-- specify exceeds the current Spot Price. Amazon EC2 periodically sets the
-- Spot Price based on available Spot Instance capacity and current Spot
-- Instance requests. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Canceling a Spot Instance request does not terminate running Spot
-- Instances associated with the request.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotInstanceRequests.html>
module Network.AWS.EC2.CancelSpotInstanceRequests
    (
    -- * Request
      CancelSpotInstanceRequests
    -- ** Request constructor
    , cancelSpotInstanceRequests
    -- ** Request lenses
    , csirDryRun
    , csirSpotInstanceRequestIds

    -- * Response
    , CancelSpotInstanceRequestsResponse
    -- ** Response constructor
    , cancelSpotInstanceRequestsResponse
    -- ** Response lenses
    , csirrsCancelledSpotInstanceRequests
    , csirrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'cancelSpotInstanceRequests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csirDryRun'
--
-- * 'csirSpotInstanceRequestIds'
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
    { _csirDryRun                 :: !(Maybe Bool)
    , _csirSpotInstanceRequestIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotInstanceRequests' smart constructor.
cancelSpotInstanceRequests :: CancelSpotInstanceRequests
cancelSpotInstanceRequests =
    CancelSpotInstanceRequests'
    { _csirDryRun = Nothing
    , _csirSpotInstanceRequestIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csirDryRun :: Lens' CancelSpotInstanceRequests (Maybe Bool)
csirDryRun = lens _csirDryRun (\ s a -> s{_csirDryRun = a});

-- | One or more Spot Instance request IDs.
csirSpotInstanceRequestIds :: Lens' CancelSpotInstanceRequests [Text]
csirSpotInstanceRequestIds = lens _csirSpotInstanceRequestIds (\ s a -> s{_csirSpotInstanceRequestIds = a});

instance AWSRequest CancelSpotInstanceRequests where
        type Sv CancelSpotInstanceRequests = EC2
        type Rs CancelSpotInstanceRequests =
             CancelSpotInstanceRequestsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CancelSpotInstanceRequestsResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders CancelSpotInstanceRequests where
        toHeaders = const mempty

instance ToPath CancelSpotInstanceRequests where
        toPath = const "/"

instance ToQuery CancelSpotInstanceRequests where
        toQuery CancelSpotInstanceRequests'{..}
          = mconcat
              ["Action" =:
                 ("CancelSpotInstanceRequests" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _csirDryRun,
               toQueryList "SpotInstanceRequestId"
                 _csirSpotInstanceRequestIds]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'cancelSpotInstanceRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csirrsCancelledSpotInstanceRequests'
--
-- * 'csirrsStatus'
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
    { _csirrsCancelledSpotInstanceRequests :: !(Maybe [CancelledSpotInstanceRequest])
    , _csirrsStatus                        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelSpotInstanceRequestsResponse' smart constructor.
cancelSpotInstanceRequestsResponse :: Int -> CancelSpotInstanceRequestsResponse
cancelSpotInstanceRequestsResponse pStatus_ =
    CancelSpotInstanceRequestsResponse'
    { _csirrsCancelledSpotInstanceRequests = Nothing
    , _csirrsStatus = pStatus_
    }

-- | One or more Spot Instance requests.
csirrsCancelledSpotInstanceRequests :: Lens' CancelSpotInstanceRequestsResponse [CancelledSpotInstanceRequest]
csirrsCancelledSpotInstanceRequests = lens _csirrsCancelledSpotInstanceRequests (\ s a -> s{_csirrsCancelledSpotInstanceRequests = a}) . _Default;

-- | FIXME: Undocumented member.
csirrsStatus :: Lens' CancelSpotInstanceRequestsResponse Int
csirrsStatus = lens _csirrsStatus (\ s a -> s{_csirrsStatus = a});

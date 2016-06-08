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
-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot instance requests. Spot instances are instances that Amazon EC2 starts on your behalf when the bid price that you specify exceeds the current Spot price. Amazon EC2 periodically sets the Spot price based on available Spot instance capacity and current Spot instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Canceling a Spot instance request does not terminate running Spot instances associated with the request.
module Network.AWS.EC2.CancelSpotInstanceRequests
    (
    -- * Creating a Request
      cancelSpotInstanceRequests
    , CancelSpotInstanceRequests
    -- * Request Lenses
    , csirDryRun
    , csirSpotInstanceRequestIds

    -- * Destructuring the Response
    , cancelSpotInstanceRequestsResponse
    , CancelSpotInstanceRequestsResponse
    -- * Response Lenses
    , csirrsCancelledSpotInstanceRequests
    , csirrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'cancelSpotInstanceRequests' smart constructor.
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
    { _csirDryRun                 :: !(Maybe Bool)
    , _csirSpotInstanceRequestIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelSpotInstanceRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirDryRun'
--
-- * 'csirSpotInstanceRequestIds'
cancelSpotInstanceRequests
    :: CancelSpotInstanceRequests
cancelSpotInstanceRequests =
    CancelSpotInstanceRequests'
    { _csirDryRun = Nothing
    , _csirSpotInstanceRequestIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
csirDryRun :: Lens' CancelSpotInstanceRequests (Maybe Bool)
csirDryRun = lens _csirDryRun (\ s a -> s{_csirDryRun = a});

-- | One or more Spot instance request IDs.
csirSpotInstanceRequestIds :: Lens' CancelSpotInstanceRequests [Text]
csirSpotInstanceRequestIds = lens _csirSpotInstanceRequestIds (\ s a -> s{_csirSpotInstanceRequestIds = a}) . _Coerce;

instance AWSRequest CancelSpotInstanceRequests where
        type Rs CancelSpotInstanceRequests =
             CancelSpotInstanceRequestsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CancelSpotInstanceRequestsResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable CancelSpotInstanceRequests

instance NFData CancelSpotInstanceRequests

instance ToHeaders CancelSpotInstanceRequests where
        toHeaders = const mempty

instance ToPath CancelSpotInstanceRequests where
        toPath = const "/"

instance ToQuery CancelSpotInstanceRequests where
        toQuery CancelSpotInstanceRequests'{..}
          = mconcat
              ["Action" =:
                 ("CancelSpotInstanceRequests" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _csirDryRun,
               toQueryList "SpotInstanceRequestId"
                 _csirSpotInstanceRequestIds]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'cancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
    { _csirrsCancelledSpotInstanceRequests :: !(Maybe [CancelledSpotInstanceRequest])
    , _csirrsResponseStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelSpotInstanceRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirrsCancelledSpotInstanceRequests'
--
-- * 'csirrsResponseStatus'
cancelSpotInstanceRequestsResponse
    :: Int -- ^ 'csirrsResponseStatus'
    -> CancelSpotInstanceRequestsResponse
cancelSpotInstanceRequestsResponse pResponseStatus_ =
    CancelSpotInstanceRequestsResponse'
    { _csirrsCancelledSpotInstanceRequests = Nothing
    , _csirrsResponseStatus = pResponseStatus_
    }

-- | One or more Spot instance requests.
csirrsCancelledSpotInstanceRequests :: Lens' CancelSpotInstanceRequestsResponse [CancelledSpotInstanceRequest]
csirrsCancelledSpotInstanceRequests = lens _csirrsCancelledSpotInstanceRequests (\ s a -> s{_csirrsCancelledSpotInstanceRequests = a}) . _Default . _Coerce;

-- | The response status code.
csirrsResponseStatus :: Lens' CancelSpotInstanceRequestsResponse Int
csirrsResponseStatus = lens _csirrsResponseStatus (\ s a -> s{_csirrsResponseStatus = a});

instance NFData CancelSpotInstanceRequestsResponse

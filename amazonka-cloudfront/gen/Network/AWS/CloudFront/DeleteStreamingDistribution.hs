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
-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution. To delete an RTMP distribution using the CloudFront API, perform the following steps.
--
--
-- __To delete an RTMP distribution using the CloudFront API__ :
--
--     * Disable the RTMP distribution.
--
--     * Submit a @GET Streaming Distribution Config@ request to get the current configuration and the @Etag@ header for the distribution.
--
--     * Update the XML document that was returned in the response to your @GET Streaming Distribution Config@ request to change the value of @Enabled@ to @false@ .
--
--     * Submit a @PUT Streaming Distribution Config@ request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Then set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Streaming Distribution Config@ request in Step 2.
--
--     * Review the response to the @PUT Streaming Distribution Config@ request to confirm that the distribution was successfully disabled.
--
--     * Submit a @GET Streaming Distribution Config@ request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
--
--     * Submit a @DELETE Streaming Distribution@ request. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Streaming Distribution Config@ request in Step 2.
--
--     * Review the response to your @DELETE Streaming Distribution@ request to confirm that the distribution was successfully deleted.
--
--
--
-- For information about deleting a distribution using the CloudFront console, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution> in the /Amazon CloudFront Developer Guide/ .
--
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Creating a Request
      deleteStreamingDistribution
    , DeleteStreamingDistribution
    -- * Request Lenses
    , dsdIfMatch
    , dsdId

    -- * Destructuring the Response
    , deleteStreamingDistributionResponse
    , DeleteStreamingDistributionResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to delete a streaming distribution.
--
--
--
-- /See:/ 'deleteStreamingDistribution' smart constructor.
data DeleteStreamingDistribution = DeleteStreamingDistribution'
  { _dsdIfMatch :: !(Maybe Text)
  , _dsdId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdIfMatch' - The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'dsdId' - The distribution ID.
deleteStreamingDistribution
    :: Text -- ^ 'dsdId'
    -> DeleteStreamingDistribution
deleteStreamingDistribution pId_ =
  DeleteStreamingDistribution' {_dsdIfMatch = Nothing, _dsdId = pId_}


-- | The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\ s a -> s{_dsdIfMatch = a})

-- | The distribution ID.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\ s a -> s{_dsdId = a})

instance AWSRequest DeleteStreamingDistribution where
        type Rs DeleteStreamingDistribution =
             DeleteStreamingDistributionResponse
        request = delete cloudFront
        response
          = receiveNull DeleteStreamingDistributionResponse'

instance Hashable DeleteStreamingDistribution where

instance NFData DeleteStreamingDistribution where

instance ToHeaders DeleteStreamingDistribution where
        toHeaders DeleteStreamingDistribution'{..}
          = mconcat ["If-Match" =# _dsdIfMatch]

instance ToPath DeleteStreamingDistribution where
        toPath DeleteStreamingDistribution'{..}
          = mconcat
              ["/2017-10-30/streaming-distribution/", toBS _dsdId]

instance ToQuery DeleteStreamingDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse =
  DeleteStreamingDistributionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamingDistributionResponse' with the minimum fields required to make a request.
--
deleteStreamingDistributionResponse
    :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'


instance NFData DeleteStreamingDistributionResponse
         where

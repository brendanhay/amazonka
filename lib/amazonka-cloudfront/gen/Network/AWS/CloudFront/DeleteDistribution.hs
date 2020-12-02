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
-- Module      : Network.AWS.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a distribution.
--
--
module Network.AWS.CloudFront.DeleteDistribution
    (
    -- * Creating a Request
      deleteDistribution
    , DeleteDistribution
    -- * Request Lenses
    , ddIfMatch
    , ddId

    -- * Destructuring the Response
    , deleteDistributionResponse
    , DeleteDistributionResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This action deletes a web distribution. To delete a web distribution using the CloudFront API, perform the following steps.
--
--
-- __To delete a web distribution using the CloudFront API:__
--
--     * Disable the web distribution
--
--     * Submit a @GET Distribution Config@ request to get the current configuration and the @Etag@ header for the distribution.
--
--     * Update the XML document that was returned in the response to your @GET Distribution Config@ request to change the value of @Enabled@ to @false@ .
--
--     * Submit a @PUT Distribution Config@ request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Distribution Config@ request in Step 2.
--
--     * Review the response to the @PUT Distribution Config@ request to confirm that the distribution was successfully disabled.
--
--     * Submit a @GET Distribution@ request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
--
--     * Submit a @DELETE Distribution@ request. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Distribution Config@ request in Step 6.
--
--     * Review the response to your @DELETE Distribution@ request to confirm that the distribution was successfully deleted.
--
--
--
-- For information about deleting a distribution using the CloudFront console, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'deleteDistribution' smart constructor.
data DeleteDistribution = DeleteDistribution'
  { _ddIfMatch :: !(Maybe Text)
  , _ddId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddIfMatch' - The value of the @ETag@ header that you received when you disabled the distribution. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'ddId' - The distribution ID.
deleteDistribution
    :: Text -- ^ 'ddId'
    -> DeleteDistribution
deleteDistribution pId_ =
  DeleteDistribution' {_ddIfMatch = Nothing, _ddId = pId_}


-- | The value of the @ETag@ header that you received when you disabled the distribution. For example: @E2QWRUHAPOMQZL@ .
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\ s a -> s{_ddIfMatch = a})

-- | The distribution ID.
ddId :: Lens' DeleteDistribution Text
ddId = lens _ddId (\ s a -> s{_ddId = a})

instance AWSRequest DeleteDistribution where
        type Rs DeleteDistribution =
             DeleteDistributionResponse
        request = delete cloudFront
        response = receiveNull DeleteDistributionResponse'

instance Hashable DeleteDistribution where

instance NFData DeleteDistribution where

instance ToHeaders DeleteDistribution where
        toHeaders DeleteDistribution'{..}
          = mconcat ["If-Match" =# _ddIfMatch]

instance ToPath DeleteDistribution where
        toPath DeleteDistribution'{..}
          = mconcat ["/2017-10-30/distribution/", toBS _ddId]

instance ToQuery DeleteDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteDistributionResponse' smart constructor.
data DeleteDistributionResponse =
  DeleteDistributionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDistributionResponse' with the minimum fields required to make a request.
--
deleteDistributionResponse
    :: DeleteDistributionResponse
deleteDistributionResponse = DeleteDistributionResponse'


instance NFData DeleteDistributionResponse where

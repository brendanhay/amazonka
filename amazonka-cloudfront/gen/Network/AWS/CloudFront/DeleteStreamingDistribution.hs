{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
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

-- | Delete a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteStreamingDistribution.html>
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Request
      DeleteStreamingDistribution
    -- ** Request constructor
    , deleteStreamingDistribution
    -- ** Request lenses
    , dsdIfMatch
    , dsdId

    -- * Response
    , DeleteStreamingDistributionResponse
    -- ** Response constructor
    , deleteStreamingDistributionResponse
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to delete a streaming distribution.
--
-- /See:/ 'deleteStreamingDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdIfMatch'
--
-- * 'dsdId'
data DeleteStreamingDistribution = DeleteStreamingDistribution'
    { _dsdIfMatch :: !(Maybe Text)
    , _dsdId      :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteStreamingDistribution' smart constructor.
deleteStreamingDistribution :: Text -> DeleteStreamingDistribution
deleteStreamingDistribution pId =
    DeleteStreamingDistribution'
    { _dsdIfMatch = Nothing
    , _dsdId = pId
    }

-- | The value of the ETag header you received when you disabled the
-- streaming distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\ s a -> s{_dsdIfMatch = a});

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\ s a -> s{_dsdId = a});

instance AWSRequest DeleteStreamingDistribution where
        type Sv DeleteStreamingDistribution = CloudFront
        type Rs DeleteStreamingDistribution =
             DeleteStreamingDistributionResponse
        request = delete
        response
          = receiveNull DeleteStreamingDistributionResponse'

instance ToHeaders DeleteStreamingDistribution where
        toHeaders DeleteStreamingDistribution'{..}
          = mconcat ["If-Match" =# _dsdIfMatch]

instance ToPath DeleteStreamingDistribution where
        toPath DeleteStreamingDistribution'{..}
          = mconcat
              ["/2015-04-17/streaming-distribution/",
               toText _dsdId]

instance ToQuery DeleteStreamingDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse =
    DeleteStreamingDistributionResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteStreamingDistributionResponse' smart constructor.
deleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'

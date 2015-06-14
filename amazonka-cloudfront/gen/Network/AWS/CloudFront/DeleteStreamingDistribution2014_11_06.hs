{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteStreamingDistribution2014_11_06.html>
module Network.AWS.CloudFront.DeleteStreamingDistribution2014_11_06
    (
    -- * Request
      DeleteStreamingDistribution2014_11_06
    -- ** Request constructor
    , deleteStreamingDistribution2014_11_06
    -- ** Request lenses
    , dsdIfMatch
    , dsdId

    -- * Response
    , DeleteStreamingDistribution2014_11_06Response
    -- ** Response constructor
    , deleteStreamingDistribution2014_11_06Response
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'deleteStreamingDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdIfMatch'
--
-- * 'dsdId'
data DeleteStreamingDistribution2014_11_06 = DeleteStreamingDistribution2014_11_06'{_dsdIfMatch :: Maybe Text, _dsdId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteStreamingDistribution2014_11_06' smart constructor.
deleteStreamingDistribution2014_11_06 :: Text -> DeleteStreamingDistribution2014_11_06
deleteStreamingDistribution2014_11_06 pId = DeleteStreamingDistribution2014_11_06'{_dsdIfMatch = Nothing, _dsdId = pId};

-- | The value of the ETag header you received when you disabled the
-- streaming distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution2014_11_06 (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\ s a -> s{_dsdIfMatch = a});

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution2014_11_06 Text
dsdId = lens _dsdId (\ s a -> s{_dsdId = a});

instance AWSRequest
         DeleteStreamingDistribution2014_11_06 where
        type Sv DeleteStreamingDistribution2014_11_06 =
             CloudFront
        type Rs DeleteStreamingDistribution2014_11_06 =
             DeleteStreamingDistribution2014_11_06Response
        request = delete
        response
          = receiveNull
              DeleteStreamingDistribution2014_11_06Response'

instance ToHeaders
         DeleteStreamingDistribution2014_11_06 where
        toHeaders DeleteStreamingDistribution2014_11_06'{..}
          = mconcat ["If-Match" =# _dsdIfMatch]

instance ToPath DeleteStreamingDistribution2014_11_06
         where
        toPath DeleteStreamingDistribution2014_11_06'{..}
          = mconcat
              ["/2014-11-06/streaming-distribution/",
               toText _dsdId]

instance ToQuery
         DeleteStreamingDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistribution2014_11_06Response' smart constructor.
data DeleteStreamingDistribution2014_11_06Response = DeleteStreamingDistribution2014_11_06Response' deriving (Eq, Read, Show)

-- | 'DeleteStreamingDistribution2014_11_06Response' smart constructor.
deleteStreamingDistribution2014_11_06Response :: DeleteStreamingDistribution2014_11_06Response
deleteStreamingDistribution2014_11_06Response = DeleteStreamingDistribution2014_11_06Response';

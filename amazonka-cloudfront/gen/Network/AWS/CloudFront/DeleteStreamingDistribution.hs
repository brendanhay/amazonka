{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteStreamingDistribution.html>
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Request
      DeleteStreamingDistribution
    -- ** Request constructor
    , deleteStreamingDistribution
    -- ** Request lenses
    , dsdrqIfMatch
    , dsdrqId

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
-- * 'dsdrqIfMatch'
--
-- * 'dsdrqId'
data DeleteStreamingDistribution = DeleteStreamingDistribution'
    { _dsdrqIfMatch :: !(Maybe Text)
    , _dsdrqId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStreamingDistribution' smart constructor.
deleteStreamingDistribution :: Text -> DeleteStreamingDistribution
deleteStreamingDistribution pId =
    DeleteStreamingDistribution'
    { _dsdrqIfMatch = Nothing
    , _dsdrqId = pId
    }

-- | The value of the ETag header you received when you disabled the
-- streaming distribution. For example: E2QWRUHAPOMQZL.
dsdrqIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdrqIfMatch = lens _dsdrqIfMatch (\ s a -> s{_dsdrqIfMatch = a});

-- | The distribution id.
dsdrqId :: Lens' DeleteStreamingDistribution Text
dsdrqId = lens _dsdrqId (\ s a -> s{_dsdrqId = a});

instance AWSRequest DeleteStreamingDistribution where
        type Sv DeleteStreamingDistribution = CloudFront
        type Rs DeleteStreamingDistribution =
             DeleteStreamingDistributionResponse
        request = delete
        response
          = receiveNull DeleteStreamingDistributionResponse'

instance ToHeaders DeleteStreamingDistribution where
        toHeaders DeleteStreamingDistribution'{..}
          = mconcat ["If-Match" =# _dsdrqIfMatch]

instance ToPath DeleteStreamingDistribution where
        toPath DeleteStreamingDistribution'{..}
          = mconcat
              ["/2015-04-17/streaming-distribution/",
               toText _dsdrqId]

instance ToQuery DeleteStreamingDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse =
    DeleteStreamingDistributionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStreamingDistributionResponse' smart constructor.
deleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'

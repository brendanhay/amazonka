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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteStreamingDistribution.html AWS API Reference> for DeleteStreamingDistribution.
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Creating a Request
      DeleteStreamingDistribution
    , deleteStreamingDistribution
    -- * Request Lenses
    , dsdIfMatch
    , dsdId

    -- * Destructuring the Response
    , DeleteStreamingDistributionResponse
    , deleteStreamingDistributionResponse
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStreamingDistribution' smart constructor.
deleteStreamingDistribution :: Text -> DeleteStreamingDistribution
deleteStreamingDistribution pId_ =
    DeleteStreamingDistribution'
    { _dsdIfMatch = Nothing
    , _dsdId = pId_
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
              ["/2015-04-17/streaming-distribution/", toBS _dsdId]

instance ToQuery DeleteStreamingDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse =
    DeleteStreamingDistributionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStreamingDistributionResponse' smart constructor.
deleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.DeleteDistribution
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

-- | Delete a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteDistribution.html>
module Network.AWS.CloudFront.DeleteDistribution
    (
    -- * Request
      DeleteDistribution
    -- ** Request constructor
    , deleteDistribution
    -- ** Request lenses
    , ddIfMatch
    , ddId

    -- * Response
    , DeleteDistributionResponse
    -- ** Response constructor
    , deleteDistributionResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddIfMatch'
--
-- * 'ddId'
data DeleteDistribution = DeleteDistribution'{_ddIfMatch :: Maybe Text, _ddId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDistribution' smart constructor.
deleteDistribution :: Text -> DeleteDistribution
deleteDistribution pId = DeleteDistribution'{_ddIfMatch = Nothing, _ddId = pId};

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\ s a -> s{_ddIfMatch = a});

-- | The distribution id.
ddId :: Lens' DeleteDistribution Text
ddId = lens _ddId (\ s a -> s{_ddId = a});

instance AWSRequest DeleteDistribution where
        type Sv DeleteDistribution = CloudFront
        type Rs DeleteDistribution =
             DeleteDistributionResponse
        request = delete
        response = receiveNull DeleteDistributionResponse'

instance ToHeaders DeleteDistribution where
        toHeaders DeleteDistribution'{..}
          = mconcat ["If-Match" =# _ddIfMatch]

instance ToPath DeleteDistribution where
        toPath DeleteDistribution'{..}
          = mconcat ["/2014-11-06/distribution/", toText _ddId]

instance ToQuery DeleteDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse' deriving (Eq, Read, Show)

-- | 'DeleteDistributionResponse' smart constructor.
deleteDistributionResponse :: DeleteDistributionResponse
deleteDistributionResponse = DeleteDistributionResponse';

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.DeleteDistribution2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteDistribution2014_11_06.html>
module Network.AWS.CloudFront.DeleteDistribution2014_11_06
    (
    -- * Request
      DeleteDistribution2014_11_06
    -- ** Request constructor
    , deleteDistribution2014_11_06
    -- ** Request lenses
    , ddIfMatch
    , ddId

    -- * Response
    , DeleteDistribution2014_11_06Response
    -- ** Response constructor
    , deleteDistribution2014_11_06Response
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'deleteDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddIfMatch'
--
-- * 'ddId'
data DeleteDistribution2014_11_06 = DeleteDistribution2014_11_06'{_ddIfMatch :: Maybe Text, _ddId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDistribution2014_11_06' smart constructor.
deleteDistribution2014_11_06 :: Text -> DeleteDistribution2014_11_06
deleteDistribution2014_11_06 pId = DeleteDistribution2014_11_06'{_ddIfMatch = Nothing, _ddId = pId};

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution2014_11_06 (Maybe Text)
ddIfMatch = lens _ddIfMatch (\ s a -> s{_ddIfMatch = a});

-- | The distribution id.
ddId :: Lens' DeleteDistribution2014_11_06 Text
ddId = lens _ddId (\ s a -> s{_ddId = a});

instance AWSRequest DeleteDistribution2014_11_06
         where
        type Sv DeleteDistribution2014_11_06 = CloudFront
        type Rs DeleteDistribution2014_11_06 =
             DeleteDistribution2014_11_06Response
        request = delete
        response
          = receiveNull DeleteDistribution2014_11_06Response'

instance ToHeaders DeleteDistribution2014_11_06 where
        toHeaders DeleteDistribution2014_11_06'{..}
          = mconcat ["If-Match" =# _ddIfMatch]

instance ToPath DeleteDistribution2014_11_06 where
        toPath DeleteDistribution2014_11_06'{..}
          = mconcat ["/2014-11-06/distribution/", toText _ddId]

instance ToQuery DeleteDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'deleteDistribution2014_11_06Response' smart constructor.
data DeleteDistribution2014_11_06Response = DeleteDistribution2014_11_06Response' deriving (Eq, Read, Show)

-- | 'DeleteDistribution2014_11_06Response' smart constructor.
deleteDistribution2014_11_06Response :: DeleteDistribution2014_11_06Response
deleteDistribution2014_11_06Response = DeleteDistribution2014_11_06Response';

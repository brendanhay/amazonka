{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Delete a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteDistribution.html>
module Network.AWS.CloudFront.DeleteDistribution
    (
    -- * Request
      DeleteDistribution
    -- ** Request constructor
    , deleteDistribution
    -- ** Request lenses
    , ddrqIfMatch
    , ddrqId

    -- * Response
    , DeleteDistributionResponse
    -- ** Response constructor
    , deleteDistributionResponse
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to delete a distribution.
--
-- /See:/ 'deleteDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrqIfMatch'
--
-- * 'ddrqId'
data DeleteDistribution = DeleteDistribution'
    { _ddrqIfMatch :: !(Maybe Text)
    , _ddrqId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDistribution' smart constructor.
deleteDistribution :: Text -> DeleteDistribution
deleteDistribution pId_ =
    DeleteDistribution'
    { _ddrqIfMatch = Nothing
    , _ddrqId = pId_
    }

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddrqIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddrqIfMatch = lens _ddrqIfMatch (\ s a -> s{_ddrqIfMatch = a});

-- | The distribution id.
ddrqId :: Lens' DeleteDistribution Text
ddrqId = lens _ddrqId (\ s a -> s{_ddrqId = a});

instance AWSRequest DeleteDistribution where
        type Sv DeleteDistribution = CloudFront
        type Rs DeleteDistribution =
             DeleteDistributionResponse
        request = delete
        response = receiveNull DeleteDistributionResponse'

instance ToHeaders DeleteDistribution where
        toHeaders DeleteDistribution'{..}
          = mconcat ["If-Match" =# _ddrqIfMatch]

instance ToPath DeleteDistribution where
        toPath DeleteDistribution'{..}
          = mconcat
              ["/2015-04-17/distribution/", toText _ddrqId]

instance ToQuery DeleteDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteDistributionResponse' smart constructor.
data DeleteDistributionResponse =
    DeleteDistributionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDistributionResponse' smart constructor.
deleteDistributionResponse :: DeleteDistributionResponse
deleteDistributionResponse = DeleteDistributionResponse'

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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteDistribution.html AWS API Reference> for DeleteDistribution.
module Network.AWS.CloudFront.DeleteDistribution
    (
    -- * Creating a Request
      DeleteDistribution
    , deleteDistribution
    -- * Request Lenses
    , ddIfMatch
    , ddId

    -- * Destructuring the Response
    , DeleteDistributionResponse
    , deleteDistributionResponse
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to delete a distribution.
--
-- /See:/ 'deleteDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddIfMatch'
--
-- * 'ddId'
data DeleteDistribution = DeleteDistribution'
    { _ddIfMatch :: !(Maybe Text)
    , _ddId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDistribution' smart constructor.
deleteDistribution :: Text -> DeleteDistribution
deleteDistribution pId_ =
    DeleteDistribution'
    { _ddIfMatch = Nothing
    , _ddId = pId_
    }

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
          = mconcat ["/2015-04-17/distribution/", toBS _ddId]

instance ToQuery DeleteDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteDistributionResponse' smart constructor.
data DeleteDistributionResponse =
    DeleteDistributionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDistributionResponse' smart constructor.
deleteDistributionResponse :: DeleteDistributionResponse
deleteDistributionResponse = DeleteDistributionResponse'

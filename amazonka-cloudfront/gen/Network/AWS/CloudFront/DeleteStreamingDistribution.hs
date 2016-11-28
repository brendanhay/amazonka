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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution.
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

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to delete a streaming distribution.
--
-- /See:/ 'deleteStreamingDistribution' smart constructor.
data DeleteStreamingDistribution = DeleteStreamingDistribution'
    { _dsdIfMatch :: !(Maybe Text)
    , _dsdId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdIfMatch' - The value of the ETag header you received when you disabled the streaming distribution. For example: E2QWRUHAPOMQZL.
--
-- * 'dsdId' - The distribution id.
deleteStreamingDistribution
    :: Text -- ^ 'dsdId'
    -> DeleteStreamingDistribution
deleteStreamingDistribution pId_ =
    DeleteStreamingDistribution'
    { _dsdIfMatch = Nothing
    , _dsdId = pId_
    }

-- | The value of the ETag header you received when you disabled the streaming distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\ s a -> s{_dsdIfMatch = a});

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\ s a -> s{_dsdId = a});

instance AWSRequest DeleteStreamingDistribution where
        type Rs DeleteStreamingDistribution =
             DeleteStreamingDistributionResponse
        request = delete cloudFront
        response
          = receiveNull DeleteStreamingDistributionResponse'

instance Hashable DeleteStreamingDistribution

instance NFData DeleteStreamingDistribution

instance ToHeaders DeleteStreamingDistribution where
        toHeaders DeleteStreamingDistribution'{..}
          = mconcat ["If-Match" =# _dsdIfMatch]

instance ToPath DeleteStreamingDistribution where
        toPath DeleteStreamingDistribution'{..}
          = mconcat
              ["/2016-09-07/streaming-distribution/", toBS _dsdId]

instance ToQuery DeleteStreamingDistribution where
        toQuery = const mempty

-- | /See:/ 'deleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse =
    DeleteStreamingDistributionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteStreamingDistributionResponse' with the minimum fields required to make a request.
--
deleteStreamingDistributionResponse
    :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'

instance NFData DeleteStreamingDistributionResponse

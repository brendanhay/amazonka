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
-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a distribution.
module Network.AWS.CloudFront.UpdateDistribution
    (
    -- * Creating a Request
      updateDistribution
    , UpdateDistribution
    -- * Request Lenses
    , udIfMatch
    , udDistributionConfig
    , udId

    -- * Destructuring the Response
    , updateDistributionResponse
    , UpdateDistributionResponse
    -- * Response Lenses
    , udrsETag
    , udrsDistribution
    , udrsResponseStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to update a distribution.
--
-- /See:/ 'updateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
    { _udIfMatch            :: !(Maybe Text)
    , _udDistributionConfig :: !DistributionConfig
    , _udId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udIfMatch' - The value of the ETag header you received when retrieving the distribution's configuration. For example: E2QWRUHAPOMQZL.
--
-- * 'udDistributionConfig' - The distribution's configuration information.
--
-- * 'udId' - The distribution's id.
updateDistribution
    :: DistributionConfig -- ^ 'udDistributionConfig'
    -> Text -- ^ 'udId'
    -> UpdateDistribution
updateDistribution pDistributionConfig_ pId_ =
    UpdateDistribution'
    { _udIfMatch = Nothing
    , _udDistributionConfig = pDistributionConfig_
    , _udId = pId_
    }

-- | The value of the ETag header you received when retrieving the distribution's configuration. For example: E2QWRUHAPOMQZL.
udIfMatch :: Lens' UpdateDistribution (Maybe Text)
udIfMatch = lens _udIfMatch (\ s a -> s{_udIfMatch = a});

-- | The distribution's configuration information.
udDistributionConfig :: Lens' UpdateDistribution DistributionConfig
udDistributionConfig = lens _udDistributionConfig (\ s a -> s{_udDistributionConfig = a});

-- | The distribution's id.
udId :: Lens' UpdateDistribution Text
udId = lens _udId (\ s a -> s{_udId = a});

instance AWSRequest UpdateDistribution where
        type Rs UpdateDistribution =
             UpdateDistributionResponse
        request = putXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 UpdateDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateDistribution

instance NFData UpdateDistribution

instance ToElement UpdateDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2016-09-07/}DistributionConfig"
              .
              _udDistributionConfig

instance ToHeaders UpdateDistribution where
        toHeaders UpdateDistribution'{..}
          = mconcat ["If-Match" =# _udIfMatch]

instance ToPath UpdateDistribution where
        toPath UpdateDistribution'{..}
          = mconcat
              ["/2016-09-07/distribution/", toBS _udId, "/config"]

instance ToQuery UpdateDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'updateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
    { _udrsETag           :: !(Maybe Text)
    , _udrsDistribution   :: !(Maybe Distribution)
    , _udrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsETag' - The current version of the configuration. For example: E2QWRUHAPOMQZL.
--
-- * 'udrsDistribution' - The distribution's information.
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDistributionResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDistributionResponse
updateDistributionResponse pResponseStatus_ =
    UpdateDistributionResponse'
    { _udrsETag = Nothing
    , _udrsDistribution = Nothing
    , _udrsResponseStatus = pResponseStatus_
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrsETag :: Lens' UpdateDistributionResponse (Maybe Text)
udrsETag = lens _udrsETag (\ s a -> s{_udrsETag = a});

-- | The distribution's information.
udrsDistribution :: Lens' UpdateDistributionResponse (Maybe Distribution)
udrsDistribution = lens _udrsDistribution (\ s a -> s{_udrsDistribution = a});

-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDistributionResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a});

instance NFData UpdateDistributionResponse

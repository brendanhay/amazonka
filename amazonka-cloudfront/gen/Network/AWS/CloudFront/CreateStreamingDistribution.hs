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
-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new streaming distribution.
module Network.AWS.CloudFront.CreateStreamingDistribution
    (
    -- * Creating a Request
      createStreamingDistribution
    , CreateStreamingDistribution
    -- * Request Lenses
    , csdStreamingDistributionConfig

    -- * Destructuring the Response
    , createStreamingDistributionResponse
    , CreateStreamingDistributionResponse
    -- * Response Lenses
    , csdrsETag
    , csdrsLocation
    , csdrsStreamingDistribution
    , csdrsResponseStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create a new streaming distribution.
--
-- /See:/ 'createStreamingDistribution' smart constructor.
newtype CreateStreamingDistribution = CreateStreamingDistribution'
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdStreamingDistributionConfig'
createStreamingDistribution
    :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
    -> CreateStreamingDistribution
createStreamingDistribution pStreamingDistributionConfig_ =
    CreateStreamingDistribution'
    { _csdStreamingDistributionConfig = pStreamingDistributionConfig_
    }

-- | The streaming distribution\'s configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig = lens _csdStreamingDistributionConfig (\ s a -> s{_csdStreamingDistributionConfig = a});

instance AWSRequest CreateStreamingDistribution where
        type Rs CreateStreamingDistribution =
             CreateStreamingDistributionResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CreateStreamingDistribution

instance NFData CreateStreamingDistribution

instance ToElement CreateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2016-01-28/}StreamingDistributionConfig"
              .
              _csdStreamingDistributionConfig

instance ToHeaders CreateStreamingDistribution where
        toHeaders = const mempty

instance ToPath CreateStreamingDistribution where
        toPath = const "/2016-01-28/streaming-distribution"

instance ToQuery CreateStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createStreamingDistributionResponse' smart constructor.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
    { _csdrsETag                  :: !(Maybe Text)
    , _csdrsLocation              :: !(Maybe Text)
    , _csdrsStreamingDistribution :: !(Maybe StreamingDistribution)
    , _csdrsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdrsETag'
--
-- * 'csdrsLocation'
--
-- * 'csdrsStreamingDistribution'
--
-- * 'csdrsResponseStatus'
createStreamingDistributionResponse
    :: Int -- ^ 'csdrsResponseStatus'
    -> CreateStreamingDistributionResponse
createStreamingDistributionResponse pResponseStatus_ =
    CreateStreamingDistributionResponse'
    { _csdrsETag = Nothing
    , _csdrsLocation = Nothing
    , _csdrsStreamingDistribution = Nothing
    , _csdrsResponseStatus = pResponseStatus_
    }

-- | The current version of the streaming distribution created.
csdrsETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsETag = lens _csdrsETag (\ s a -> s{_csdrsETag = a});

-- | The fully qualified URI of the new streaming distribution resource just created. For example: https:\/\/cloudfront.amazonaws.com\/2010-11-01\/streaming-distribution\/EGTXBD79H29TRA8.
csdrsLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsLocation = lens _csdrsLocation (\ s a -> s{_csdrsLocation = a});

-- | The streaming distribution\'s information.
csdrsStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrsStreamingDistribution = lens _csdrsStreamingDistribution (\ s a -> s{_csdrsStreamingDistribution = a});

-- | The response status code.
csdrsResponseStatus :: Lens' CreateStreamingDistributionResponse Int
csdrsResponseStatus = lens _csdrsResponseStatus (\ s a -> s{_csdrsResponseStatus = a});

instance NFData CreateStreamingDistributionResponse

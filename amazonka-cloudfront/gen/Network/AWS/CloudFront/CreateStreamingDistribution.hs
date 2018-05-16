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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new RMTP distribution. An RTMP distribution is similar to a web distribution, but an RTMP distribution streams media files using the Adobe Real-Time Messaging Protocol (RTMP) instead of serving files using HTTP.
--
--
-- To create a new web distribution, submit a @POST@ request to the /CloudFront API version/ /distribution resource. The request body must include a document with a /StreamingDistributionConfig/ element. The response echoes the @StreamingDistributionConfig@ element and returns other information about the RTMP distribution.
--
-- To get the status of your request, use the /GET StreamingDistribution/ API action. When the value of @Enabled@ is @true@ and the value of @Status@ is @Deployed@ , your distribution is ready. A distribution usually deploys in less than 15 minutes.
--
-- For more information about web distributions, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-rtmp.html Working with RTMP Distributions> in the /Amazon CloudFront Developer Guide/ .
--
-- /Important:/ Beginning with the 2012-05-05 version of the CloudFront API, we made substantial changes to the format of the XML document that you include in the request body when you create or update a web distribution or an RTMP distribution, and when you invalidate objects. With previous versions of the API, we discovered that it was too easy to accidentally delete one or more values for an element that accepts multiple values, for example, CNAMEs and trusted signers. Our changes for the 2012-05-05 release are intended to prevent these accidental deletions and to notify you when there's a mismatch between the number of values you say you're specifying in the @Quantity@ element and the number of values specified.
--
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

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to create a new streaming distribution.
--
--
--
-- /See:/ 'createStreamingDistribution' smart constructor.
newtype CreateStreamingDistribution = CreateStreamingDistribution'
  { _csdStreamingDistributionConfig :: StreamingDistributionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdStreamingDistributionConfig' - The streaming distribution's configuration information.
createStreamingDistribution
    :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
    -> CreateStreamingDistribution
createStreamingDistribution pStreamingDistributionConfig_ =
  CreateStreamingDistribution'
    {_csdStreamingDistributionConfig = pStreamingDistributionConfig_}


-- | The streaming distribution's configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig = lens _csdStreamingDistributionConfig (\ s a -> s{_csdStreamingDistributionConfig = a})

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

instance Hashable CreateStreamingDistribution where

instance NFData CreateStreamingDistribution where

instance ToElement CreateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}StreamingDistributionConfig"
              .
              _csdStreamingDistributionConfig

instance ToHeaders CreateStreamingDistribution where
        toHeaders = const mempty

instance ToPath CreateStreamingDistribution where
        toPath = const "/2017-10-30/streaming-distribution"

instance ToQuery CreateStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'createStreamingDistributionResponse' smart constructor.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
  { _csdrsETag                  :: !(Maybe Text)
  , _csdrsLocation              :: !(Maybe Text)
  , _csdrsStreamingDistribution :: !(Maybe StreamingDistribution)
  , _csdrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdrsETag' - The current version of the streaming distribution created.
--
-- * 'csdrsLocation' - The fully qualified URI of the new streaming distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8@ .
--
-- * 'csdrsStreamingDistribution' - The streaming distribution's information.
--
-- * 'csdrsResponseStatus' - -- | The response status code.
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
csdrsETag = lens _csdrsETag (\ s a -> s{_csdrsETag = a})

-- | The fully qualified URI of the new streaming distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8@ .
csdrsLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsLocation = lens _csdrsLocation (\ s a -> s{_csdrsLocation = a})

-- | The streaming distribution's information.
csdrsStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrsStreamingDistribution = lens _csdrsStreamingDistribution (\ s a -> s{_csdrsStreamingDistribution = a})

-- | -- | The response status code.
csdrsResponseStatus :: Lens' CreateStreamingDistributionResponse Int
csdrsResponseStatus = lens _csdrsResponseStatus (\ s a -> s{_csdrsResponseStatus = a})

instance NFData CreateStreamingDistributionResponse
         where

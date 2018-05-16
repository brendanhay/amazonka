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
-- Module      : Network.AWS.CloudFront.CreateStreamingDistributionWithTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new streaming distribution with tags.
--
--
module Network.AWS.CloudFront.CreateStreamingDistributionWithTags
    (
    -- * Creating a Request
      createStreamingDistributionWithTags
    , CreateStreamingDistributionWithTags
    -- * Request Lenses
    , csdwtStreamingDistributionConfigWithTags

    -- * Destructuring the Response
    , createStreamingDistributionWithTagsResponse
    , CreateStreamingDistributionWithTagsResponse
    -- * Response Lenses
    , csdwtrsETag
    , csdwtrsLocation
    , csdwtrsStreamingDistribution
    , csdwtrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to create a new streaming distribution with tags.
--
--
--
-- /See:/ 'createStreamingDistributionWithTags' smart constructor.
newtype CreateStreamingDistributionWithTags = CreateStreamingDistributionWithTags'
  { _csdwtStreamingDistributionConfigWithTags :: StreamingDistributionConfigWithTags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamingDistributionWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdwtStreamingDistributionConfigWithTags' - The streaming distribution's configuration information.
createStreamingDistributionWithTags
    :: StreamingDistributionConfigWithTags -- ^ 'csdwtStreamingDistributionConfigWithTags'
    -> CreateStreamingDistributionWithTags
createStreamingDistributionWithTags pStreamingDistributionConfigWithTags_ =
  CreateStreamingDistributionWithTags'
    { _csdwtStreamingDistributionConfigWithTags =
        pStreamingDistributionConfigWithTags_
    }


-- | The streaming distribution's configuration information.
csdwtStreamingDistributionConfigWithTags :: Lens' CreateStreamingDistributionWithTags StreamingDistributionConfigWithTags
csdwtStreamingDistributionConfigWithTags = lens _csdwtStreamingDistributionConfigWithTags (\ s a -> s{_csdwtStreamingDistributionConfigWithTags = a})

instance AWSRequest
           CreateStreamingDistributionWithTags
         where
        type Rs CreateStreamingDistributionWithTags =
             CreateStreamingDistributionWithTagsResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateStreamingDistributionWithTagsResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CreateStreamingDistributionWithTags
         where

instance NFData CreateStreamingDistributionWithTags
         where

instance ToElement
           CreateStreamingDistributionWithTags
         where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}StreamingDistributionConfigWithTags"
              .
              _csdwtStreamingDistributionConfigWithTags

instance ToHeaders
           CreateStreamingDistributionWithTags
         where
        toHeaders = const mempty

instance ToPath CreateStreamingDistributionWithTags
         where
        toPath = const "/2017-10-30/streaming-distribution"

instance ToQuery CreateStreamingDistributionWithTags
         where
        toQuery = const (mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'createStreamingDistributionWithTagsResponse' smart constructor.
data CreateStreamingDistributionWithTagsResponse = CreateStreamingDistributionWithTagsResponse'
  { _csdwtrsETag                  :: !(Maybe Text)
  , _csdwtrsLocation              :: !(Maybe Text)
  , _csdwtrsStreamingDistribution :: !(Maybe StreamingDistribution)
  , _csdwtrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamingDistributionWithTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdwtrsETag' - Undocumented member.
--
-- * 'csdwtrsLocation' - The fully qualified URI of the new streaming distribution resource just created. For example:@https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8@ .
--
-- * 'csdwtrsStreamingDistribution' - The streaming distribution's information.
--
-- * 'csdwtrsResponseStatus' - -- | The response status code.
createStreamingDistributionWithTagsResponse
    :: Int -- ^ 'csdwtrsResponseStatus'
    -> CreateStreamingDistributionWithTagsResponse
createStreamingDistributionWithTagsResponse pResponseStatus_ =
  CreateStreamingDistributionWithTagsResponse'
    { _csdwtrsETag = Nothing
    , _csdwtrsLocation = Nothing
    , _csdwtrsStreamingDistribution = Nothing
    , _csdwtrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
csdwtrsETag :: Lens' CreateStreamingDistributionWithTagsResponse (Maybe Text)
csdwtrsETag = lens _csdwtrsETag (\ s a -> s{_csdwtrsETag = a})

-- | The fully qualified URI of the new streaming distribution resource just created. For example:@https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8@ .
csdwtrsLocation :: Lens' CreateStreamingDistributionWithTagsResponse (Maybe Text)
csdwtrsLocation = lens _csdwtrsLocation (\ s a -> s{_csdwtrsLocation = a})

-- | The streaming distribution's information.
csdwtrsStreamingDistribution :: Lens' CreateStreamingDistributionWithTagsResponse (Maybe StreamingDistribution)
csdwtrsStreamingDistribution = lens _csdwtrsStreamingDistribution (\ s a -> s{_csdwtrsStreamingDistribution = a})

-- | -- | The response status code.
csdwtrsResponseStatus :: Lens' CreateStreamingDistributionWithTagsResponse Int
csdwtrsResponseStatus = lens _csdwtrsResponseStatus (\ s a -> s{_csdwtrsResponseStatus = a})

instance NFData
           CreateStreamingDistributionWithTagsResponse
         where

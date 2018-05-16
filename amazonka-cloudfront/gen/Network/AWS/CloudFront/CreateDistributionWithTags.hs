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
-- Module      : Network.AWS.CloudFront.CreateDistributionWithTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution with tags.
--
--
module Network.AWS.CloudFront.CreateDistributionWithTags
    (
    -- * Creating a Request
      createDistributionWithTags
    , CreateDistributionWithTags
    -- * Request Lenses
    , cdwtDistributionConfigWithTags

    -- * Destructuring the Response
    , createDistributionWithTagsResponse
    , CreateDistributionWithTagsResponse
    -- * Response Lenses
    , cdwtrsETag
    , cdwtrsDistribution
    , cdwtrsLocation
    , cdwtrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to create a new distribution with tags.
--
--
--
-- /See:/ 'createDistributionWithTags' smart constructor.
newtype CreateDistributionWithTags = CreateDistributionWithTags'
  { _cdwtDistributionConfigWithTags :: DistributionConfigWithTags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDistributionWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdwtDistributionConfigWithTags' - The distribution's configuration information.
createDistributionWithTags
    :: DistributionConfigWithTags -- ^ 'cdwtDistributionConfigWithTags'
    -> CreateDistributionWithTags
createDistributionWithTags pDistributionConfigWithTags_ =
  CreateDistributionWithTags'
    {_cdwtDistributionConfigWithTags = pDistributionConfigWithTags_}


-- | The distribution's configuration information.
cdwtDistributionConfigWithTags :: Lens' CreateDistributionWithTags DistributionConfigWithTags
cdwtDistributionConfigWithTags = lens _cdwtDistributionConfigWithTags (\ s a -> s{_cdwtDistributionConfigWithTags = a})

instance AWSRequest CreateDistributionWithTags where
        type Rs CreateDistributionWithTags =
             CreateDistributionWithTagsResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateDistributionWithTagsResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (h .#? "Location")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDistributionWithTags where

instance NFData CreateDistributionWithTags where

instance ToElement CreateDistributionWithTags where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}DistributionConfigWithTags"
              .
              _cdwtDistributionConfigWithTags

instance ToHeaders CreateDistributionWithTags where
        toHeaders = const mempty

instance ToPath CreateDistributionWithTags where
        toPath = const "/2017-10-30/distribution"

instance ToQuery CreateDistributionWithTags where
        toQuery = const (mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'createDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { _cdwtrsETag           :: !(Maybe Text)
  , _cdwtrsDistribution   :: !(Maybe Distribution)
  , _cdwtrsLocation       :: !(Maybe Text)
  , _cdwtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDistributionWithTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdwtrsETag' - The current version of the distribution created.
--
-- * 'cdwtrsDistribution' - The distribution's information.
--
-- * 'cdwtrsLocation' - The fully qualified URI of the new distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5@ .
--
-- * 'cdwtrsResponseStatus' - -- | The response status code.
createDistributionWithTagsResponse
    :: Int -- ^ 'cdwtrsResponseStatus'
    -> CreateDistributionWithTagsResponse
createDistributionWithTagsResponse pResponseStatus_ =
  CreateDistributionWithTagsResponse'
    { _cdwtrsETag = Nothing
    , _cdwtrsDistribution = Nothing
    , _cdwtrsLocation = Nothing
    , _cdwtrsResponseStatus = pResponseStatus_
    }


-- | The current version of the distribution created.
cdwtrsETag :: Lens' CreateDistributionWithTagsResponse (Maybe Text)
cdwtrsETag = lens _cdwtrsETag (\ s a -> s{_cdwtrsETag = a})

-- | The distribution's information.
cdwtrsDistribution :: Lens' CreateDistributionWithTagsResponse (Maybe Distribution)
cdwtrsDistribution = lens _cdwtrsDistribution (\ s a -> s{_cdwtrsDistribution = a})

-- | The fully qualified URI of the new distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5@ .
cdwtrsLocation :: Lens' CreateDistributionWithTagsResponse (Maybe Text)
cdwtrsLocation = lens _cdwtrsLocation (\ s a -> s{_cdwtrsLocation = a})

-- | -- | The response status code.
cdwtrsResponseStatus :: Lens' CreateDistributionWithTagsResponse Int
cdwtrsResponseStatus = lens _cdwtrsResponseStatus (\ s a -> s{_cdwtrsResponseStatus = a})

instance NFData CreateDistributionWithTagsResponse
         where

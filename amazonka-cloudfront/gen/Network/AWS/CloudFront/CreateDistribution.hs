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
-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new web distribution. Send a @POST@ request to the @//CloudFront API version/ /distribution@ /@distribution ID@ resource.
--
--
module Network.AWS.CloudFront.CreateDistribution
    (
    -- * Creating a Request
      createDistribution
    , CreateDistribution
    -- * Request Lenses
    , cdDistributionConfig

    -- * Destructuring the Response
    , createDistributionResponse
    , CreateDistributionResponse
    -- * Response Lenses
    , cdrsETag
    , cdrsDistribution
    , cdrsLocation
    , cdrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to create a new distribution.
--
--
--
-- /See:/ 'createDistribution' smart constructor.
newtype CreateDistribution = CreateDistribution'
  { _cdDistributionConfig :: DistributionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDistributionConfig' - The distribution's configuration information.
createDistribution
    :: DistributionConfig -- ^ 'cdDistributionConfig'
    -> CreateDistribution
createDistribution pDistributionConfig_ =
  CreateDistribution' {_cdDistributionConfig = pDistributionConfig_}


-- | The distribution's configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig = lens _cdDistributionConfig (\ s a -> s{_cdDistributionConfig = a})

instance AWSRequest CreateDistribution where
        type Rs CreateDistribution =
             CreateDistributionResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (h .#? "Location")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDistribution where

instance NFData CreateDistribution where

instance ToElement CreateDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}DistributionConfig"
              .
              _cdDistributionConfig

instance ToHeaders CreateDistribution where
        toHeaders = const mempty

instance ToPath CreateDistribution where
        toPath = const "/2017-10-30/distribution"

instance ToQuery CreateDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'createDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { _cdrsETag           :: !(Maybe Text)
  , _cdrsDistribution   :: !(Maybe Distribution)
  , _cdrsLocation       :: !(Maybe Text)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsETag' - The current version of the distribution created.
--
-- * 'cdrsDistribution' - The distribution's information.
--
-- * 'cdrsLocation' - The fully qualified URI of the new distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5@ .
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDistributionResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDistributionResponse
createDistributionResponse pResponseStatus_ =
  CreateDistributionResponse'
    { _cdrsETag = Nothing
    , _cdrsDistribution = Nothing
    , _cdrsLocation = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }


-- | The current version of the distribution created.
cdrsETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrsETag = lens _cdrsETag (\ s a -> s{_cdrsETag = a})

-- | The distribution's information.
cdrsDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrsDistribution = lens _cdrsDistribution (\ s a -> s{_cdrsDistribution = a})

-- | The fully qualified URI of the new distribution resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5@ .
cdrsLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrsLocation = lens _cdrsLocation (\ s a -> s{_cdrsLocation = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDistributionResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDistributionResponse where

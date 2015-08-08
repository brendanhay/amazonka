{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateDistribution.html AWS API Reference> for CreateDistribution.
module Network.AWS.CloudFront.CreateDistribution
    (
    -- * Creating a Request
      CreateDistribution
    , createDistribution
    -- * Request Lenses
    , cdDistributionConfig

    -- * Destructuring the Response
    , CreateDistributionResponse
    , createDistributionResponse
    -- * Response Lenses
    , cdrsETag
    , cdrsDistribution
    , cdrsLocation
    , cdrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create a new distribution.
--
-- /See:/ 'createDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDistributionConfig'
newtype CreateDistribution = CreateDistribution'
    { _cdDistributionConfig :: DistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDistribution' smart constructor.
createDistribution :: DistributionConfig -> CreateDistribution
createDistribution pDistributionConfig_ =
    CreateDistribution'
    { _cdDistributionConfig = pDistributionConfig_
    }

-- | The distribution\'s configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig = lens _cdDistributionConfig (\ s a -> s{_cdDistributionConfig = a});

instance AWSRequest CreateDistribution where
        type Sv CreateDistribution = CloudFront
        type Rs CreateDistribution =
             CreateDistributionResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (h .#? "Location")
                     <*> (pure (fromEnum s)))

instance ToElement CreateDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}DistributionConfig"
              .
              _cdDistributionConfig

instance ToHeaders CreateDistribution where
        toHeaders = const mempty

instance ToPath CreateDistribution where
        toPath = const "/2015-04-17/distribution"

instance ToQuery CreateDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrsETag'
--
-- * 'cdrsDistribution'
--
-- * 'cdrsLocation'
--
-- * 'cdrsStatus'
data CreateDistributionResponse = CreateDistributionResponse'
    { _cdrsETag         :: !(Maybe Text)
    , _cdrsDistribution :: !(Maybe Distribution)
    , _cdrsLocation     :: !(Maybe Text)
    , _cdrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDistributionResponse' smart constructor.
createDistributionResponse :: Int -> CreateDistributionResponse
createDistributionResponse pStatus_ =
    CreateDistributionResponse'
    { _cdrsETag = Nothing
    , _cdrsDistribution = Nothing
    , _cdrsLocation = Nothing
    , _cdrsStatus = pStatus_
    }

-- | The current version of the distribution created.
cdrsETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrsETag = lens _cdrsETag (\ s a -> s{_cdrsETag = a});

-- | The distribution\'s information.
cdrsDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrsDistribution = lens _cdrsDistribution (\ s a -> s{_cdrsDistribution = a});

-- | The fully qualified URI of the new distribution resource just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/distribution\/EDFDVBD632BHDS5.
cdrsLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrsLocation = lens _cdrsLocation (\ s a -> s{_cdrsLocation = a});

-- | Undocumented member.
cdrsStatus :: Lens' CreateDistributionResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});

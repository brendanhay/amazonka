{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistribution.html AWS API Reference> for GetDistribution.
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Creating a Request
      GetDistribution
    , getDistribution
    -- * Request Lenses
    , gdId

    -- * Destructuring the Response
    , GetDistributionResponse
    , getDistributionResponse
    -- * Response Lenses
    , gdrsETag
    , gdrsDistribution
    , gdrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get a distribution\'s information.
--
-- /See:/ 'getDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdId'
newtype GetDistribution = GetDistribution'
    { _gdId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistribution' smart constructor.
getDistribution :: Text -> GetDistribution
getDistribution pId_ =
    GetDistribution'
    { _gdId = pId_
    }

-- | The distribution\'s id.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\ s a -> s{_gdId = a});

instance AWSRequest GetDistribution where
        type Sv GetDistribution = CloudFront
        type Rs GetDistribution = GetDistributionResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDistribution where
        toHeaders = const mempty

instance ToPath GetDistribution where
        toPath GetDistribution'{..}
          = mconcat ["/2015-04-17/distribution/", toBS _gdId]

instance ToQuery GetDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrsETag'
--
-- * 'gdrsDistribution'
--
-- * 'gdrsStatus'
data GetDistributionResponse = GetDistributionResponse'
    { _gdrsETag         :: !(Maybe Text)
    , _gdrsDistribution :: !(Maybe Distribution)
    , _gdrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistributionResponse' smart constructor.
getDistributionResponse :: Int -> GetDistributionResponse
getDistributionResponse pStatus_ =
    GetDistributionResponse'
    { _gdrsETag = Nothing
    , _gdrsDistribution = Nothing
    , _gdrsStatus = pStatus_
    }

-- | The current version of the distribution\'s information. For example:
-- E2QWRUHAPOMQZL.
gdrsETag :: Lens' GetDistributionResponse (Maybe Text)
gdrsETag = lens _gdrsETag (\ s a -> s{_gdrsETag = a});

-- | The distribution\'s information.
gdrsDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrsDistribution = lens _gdrsDistribution (\ s a -> s{_gdrsDistribution = a});

-- | Undocumented member.
gdrsStatus :: Lens' GetDistributionResponse Int
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a});

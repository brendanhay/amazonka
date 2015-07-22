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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistribution.html>
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Request
      GetDistribution
    -- ** Request constructor
    , getDistribution
    -- ** Request lenses
    , gdrqId

    -- * Response
    , GetDistributionResponse
    -- ** Response constructor
    , getDistributionResponse
    -- ** Response lenses
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
-- * 'gdrqId'
newtype GetDistribution = GetDistribution'
    { _gdrqId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistribution' smart constructor.
getDistribution :: Text -> GetDistribution
getDistribution pId =
    GetDistribution'
    { _gdrqId = pId
    }

-- | The distribution\'s id.
gdrqId :: Lens' GetDistribution Text
gdrqId = lens _gdrqId (\ s a -> s{_gdrqId = a});

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
          = mconcat
              ["/2015-04-17/distribution/", toText _gdrqId]

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
getDistributionResponse pStatus =
    GetDistributionResponse'
    { _gdrsETag = Nothing
    , _gdrsDistribution = Nothing
    , _gdrsStatus = pStatus
    }

-- | The current version of the distribution\'s information. For example:
-- E2QWRUHAPOMQZL.
gdrsETag :: Lens' GetDistributionResponse (Maybe Text)
gdrsETag = lens _gdrsETag (\ s a -> s{_gdrsETag = a});

-- | The distribution\'s information.
gdrsDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrsDistribution = lens _gdrsDistribution (\ s a -> s{_gdrsDistribution = a});

-- | FIXME: Undocumented member.
gdrsStatus :: Lens' GetDistributionResponse Int
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a});

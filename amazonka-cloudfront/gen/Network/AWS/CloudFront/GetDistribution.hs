{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , gdId

    -- * Response
    , GetDistributionResponse
    -- ** Response constructor
    , getDistributionResponse
    -- ** Response lenses
    , gdrETag
    , gdrDistribution
    , gdrStatus
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
getDistribution pId =
    GetDistribution'
    { _gdId = pId
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
          = mconcat ["/2015-04-17/distribution/", toText _gdId]

instance ToQuery GetDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrETag'
--
-- * 'gdrDistribution'
--
-- * 'gdrStatus'
data GetDistributionResponse = GetDistributionResponse'
    { _gdrETag         :: !(Maybe Text)
    , _gdrDistribution :: !(Maybe Distribution)
    , _gdrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistributionResponse' smart constructor.
getDistributionResponse :: Int -> GetDistributionResponse
getDistributionResponse pStatus =
    GetDistributionResponse'
    { _gdrETag = Nothing
    , _gdrDistribution = Nothing
    , _gdrStatus = pStatus
    }

-- | The current version of the distribution\'s information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistributionResponse (Maybe Text)
gdrETag = lens _gdrETag (\ s a -> s{_gdrETag = a});

-- | The distribution\'s information.
gdrDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\ s a -> s{_gdrDistribution = a});

-- | FIXME: Undocumented member.
gdrStatus :: Lens' GetDistributionResponse Int
gdrStatus = lens _gdrStatus (\ s a -> s{_gdrStatus = a});

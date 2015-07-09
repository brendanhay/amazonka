{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Update a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateDistribution.html>
module Network.AWS.CloudFront.UpdateDistribution
    (
    -- * Request
      UpdateDistribution
    -- ** Request constructor
    , updateDistribution
    -- ** Request lenses
    , udIfMatch
    , udDistributionConfig
    , udId

    -- * Response
    , UpdateDistributionResponse
    -- ** Response constructor
    , updateDistributionResponse
    -- ** Response lenses
    , udrETag
    , udrDistribution
    , udrStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to update a distribution.
--
-- /See:/ 'updateDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udIfMatch'
--
-- * 'udDistributionConfig'
--
-- * 'udId'
data UpdateDistribution = UpdateDistribution'
    { _udIfMatch            :: !(Maybe Text)
    , _udDistributionConfig :: !DistributionConfig
    , _udId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDistribution' smart constructor.
updateDistribution :: DistributionConfig -> Text -> UpdateDistribution
updateDistribution pDistributionConfig pId =
    UpdateDistribution'
    { _udIfMatch = Nothing
    , _udDistributionConfig = pDistributionConfig
    , _udId = pId
    }

-- | The value of the ETag header you received when retrieving the
-- distribution\'s configuration. For example: E2QWRUHAPOMQZL.
udIfMatch :: Lens' UpdateDistribution (Maybe Text)
udIfMatch = lens _udIfMatch (\ s a -> s{_udIfMatch = a});

-- | The distribution\'s configuration information.
udDistributionConfig :: Lens' UpdateDistribution DistributionConfig
udDistributionConfig = lens _udDistributionConfig (\ s a -> s{_udDistributionConfig = a});

-- | The distribution\'s id.
udId :: Lens' UpdateDistribution Text
udId = lens _udId (\ s a -> s{_udId = a});

instance AWSRequest UpdateDistribution where
        type Sv UpdateDistribution = CloudFront
        type Rs UpdateDistribution =
             UpdateDistributionResponse
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateDistributionResponse' <$>
                   (h .#? "ETag") <*> (x .@? "Distribution") <*>
                     (pure (fromEnum s)))

instance ToElement UpdateDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}DistributionConfig"
              .
              _udDistributionConfig

instance ToHeaders UpdateDistribution where
        toHeaders UpdateDistribution'{..}
          = mconcat ["If-Match" =# _udIfMatch]

instance ToPath UpdateDistribution where
        toPath UpdateDistribution'{..}
          = mconcat
              ["/2015-04-17/distribution/", toText _udId,
               "/config"]

instance ToQuery UpdateDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'updateDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrETag'
--
-- * 'udrDistribution'
--
-- * 'udrStatus'
data UpdateDistributionResponse = UpdateDistributionResponse'
    { _udrETag         :: !(Maybe Text)
    , _udrDistribution :: !(Maybe Distribution)
    , _udrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDistributionResponse' smart constructor.
updateDistributionResponse :: Int -> UpdateDistributionResponse
updateDistributionResponse pStatus =
    UpdateDistributionResponse'
    { _udrETag = Nothing
    , _udrDistribution = Nothing
    , _udrStatus = pStatus
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrETag :: Lens' UpdateDistributionResponse (Maybe Text)
udrETag = lens _udrETag (\ s a -> s{_udrETag = a});

-- | The distribution\'s information.
udrDistribution :: Lens' UpdateDistributionResponse (Maybe Distribution)
udrDistribution = lens _udrDistribution (\ s a -> s{_udrDistribution = a});

-- | FIXME: Undocumented member.
udrStatus :: Lens' UpdateDistributionResponse Int
udrStatus = lens _udrStatus (\ s a -> s{_udrStatus = a});

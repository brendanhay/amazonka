{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetInvalidation2014_11_06
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Get the information about an invalidation.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetInvalidation2014_11_06.html>
module Network.AWS.CloudFront.GetInvalidation2014_11_06
    (
    -- * Request
      GetInvalidation2014_11_06
    -- ** Request constructor
    , getInvalidation2014_11_06
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidation2014_11_06Response
    -- ** Response constructor
    , getInvalidation2014_11_06Response
    -- ** Response lenses
    , girInvalidation
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getInvalidation2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giDistributionId'
--
-- * 'giId'
data GetInvalidation2014_11_06 = GetInvalidation2014_11_06'{_giDistributionId :: Text, _giId :: Text} deriving (Eq, Read, Show)

-- | 'GetInvalidation2014_11_06' smart constructor.
getInvalidation2014_11_06 :: Text -> Text -> GetInvalidation2014_11_06
getInvalidation2014_11_06 pDistributionId pId = GetInvalidation2014_11_06'{_giDistributionId = pDistributionId, _giId = pId};

-- | The distribution\'s id.
giDistributionId :: Lens' GetInvalidation2014_11_06 Text
giDistributionId = lens _giDistributionId (\ s a -> s{_giDistributionId = a});

-- | The invalidation\'s id.
giId :: Lens' GetInvalidation2014_11_06 Text
giId = lens _giId (\ s a -> s{_giId = a});

instance AWSRequest GetInvalidation2014_11_06 where
        type Sv GetInvalidation2014_11_06 = CloudFront
        type Rs GetInvalidation2014_11_06 =
             GetInvalidation2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetInvalidation2014_11_06Response' <$>
                   x .@? "Invalidation")

instance ToHeaders GetInvalidation2014_11_06 where
        toHeaders = const mempty

instance ToPath GetInvalidation2014_11_06 where
        toPath GetInvalidation2014_11_06'{..}
          = mconcat
              ["/2014-11-06/distribution/",
               toText _giDistributionId, "/invalidation/",
               toText _giId]

instance ToQuery GetInvalidation2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'getInvalidation2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girInvalidation'
newtype GetInvalidation2014_11_06Response = GetInvalidation2014_11_06Response'{_girInvalidation :: Maybe Invalidation} deriving (Eq, Read, Show)

-- | 'GetInvalidation2014_11_06Response' smart constructor.
getInvalidation2014_11_06Response :: GetInvalidation2014_11_06Response
getInvalidation2014_11_06Response = GetInvalidation2014_11_06Response'{_girInvalidation = Nothing};

-- | The invalidation\'s information.
girInvalidation :: Lens' GetInvalidation2014_11_06Response (Maybe Invalidation)
girInvalidation = lens _girInvalidation (\ s a -> s{_girInvalidation = a});

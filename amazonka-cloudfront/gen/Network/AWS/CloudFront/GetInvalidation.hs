{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetInvalidation
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetInvalidation.html>
module Network.AWS.CloudFront.GetInvalidation
    (
    -- * Request
      GetInvalidation
    -- ** Request constructor
    , getInvalidation
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidationResponse
    -- ** Response constructor
    , getInvalidationResponse
    -- ** Response lenses
    , girInvalidation
    , girStatusCode
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to get an invalidation\'s information.
--
-- /See:/ 'getInvalidation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giDistributionId'
--
-- * 'giId'
data GetInvalidation = GetInvalidation'{_giDistributionId :: Text, _giId :: Text} deriving (Eq, Read, Show)

-- | 'GetInvalidation' smart constructor.
getInvalidation :: Text -> Text -> GetInvalidation
getInvalidation pDistributionId pId = GetInvalidation'{_giDistributionId = pDistributionId, _giId = pId};

-- | The distribution\'s id.
giDistributionId :: Lens' GetInvalidation Text
giDistributionId = lens _giDistributionId (\ s a -> s{_giDistributionId = a});

-- | The invalidation\'s id.
giId :: Lens' GetInvalidation Text
giId = lens _giId (\ s a -> s{_giId = a});

instance AWSRequest GetInvalidation where
        type Sv GetInvalidation = CloudFront
        type Rs GetInvalidation = GetInvalidationResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetInvalidationResponse' <$>
                   (x .@? "Invalidation") <*> (pure (fromEnum s)))

instance ToHeaders GetInvalidation where
        toHeaders = const mempty

instance ToPath GetInvalidation where
        toPath GetInvalidation'{..}
          = mconcat
              ["/2014-11-06/distribution/",
               toText _giDistributionId, "/invalidation/",
               toText _giId]

instance ToQuery GetInvalidation where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getInvalidationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girInvalidation'
--
-- * 'girStatusCode'
data GetInvalidationResponse = GetInvalidationResponse'{_girInvalidation :: Maybe Invalidation, _girStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetInvalidationResponse' smart constructor.
getInvalidationResponse :: Int -> GetInvalidationResponse
getInvalidationResponse pStatusCode = GetInvalidationResponse'{_girInvalidation = Nothing, _girStatusCode = pStatusCode};

-- | The invalidation\'s information.
girInvalidation :: Lens' GetInvalidationResponse (Maybe Invalidation)
girInvalidation = lens _girInvalidation (\ s a -> s{_girInvalidation = a});

-- | FIXME: Undocumented member.
girStatusCode :: Lens' GetInvalidationResponse Int
girStatusCode = lens _girStatusCode (\ s a -> s{_girStatusCode = a});

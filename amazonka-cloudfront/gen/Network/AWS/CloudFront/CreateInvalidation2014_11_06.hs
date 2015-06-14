{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.CreateInvalidation2014_11_06
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

-- | Create a new invalidation.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateInvalidation2014_11_06.html>
module Network.AWS.CloudFront.CreateInvalidation2014_11_06
    (
    -- * Request
      CreateInvalidation2014_11_06
    -- ** Request constructor
    , createInvalidation2014_11_06
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Response
    , CreateInvalidation2014_11_06Response
    -- ** Response constructor
    , createInvalidation2014_11_06Response
    -- ** Response lenses
    , cirInvalidation
    , cirLocation
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'createInvalidation2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciDistributionId'
--
-- * 'ciInvalidationBatch'
data CreateInvalidation2014_11_06 = CreateInvalidation2014_11_06'{_ciDistributionId :: Text, _ciInvalidationBatch :: InvalidationBatch} deriving (Eq, Read, Show)

-- | 'CreateInvalidation2014_11_06' smart constructor.
createInvalidation2014_11_06 :: Text -> InvalidationBatch -> CreateInvalidation2014_11_06
createInvalidation2014_11_06 pDistributionId pInvalidationBatch = CreateInvalidation2014_11_06'{_ciDistributionId = pDistributionId, _ciInvalidationBatch = pInvalidationBatch};

-- | The distribution\'s id.
ciDistributionId :: Lens' CreateInvalidation2014_11_06 Text
ciDistributionId = lens _ciDistributionId (\ s a -> s{_ciDistributionId = a});

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation2014_11_06 InvalidationBatch
ciInvalidationBatch = lens _ciInvalidationBatch (\ s a -> s{_ciInvalidationBatch = a});

instance AWSRequest CreateInvalidation2014_11_06
         where
        type Sv CreateInvalidation2014_11_06 = CloudFront
        type Rs CreateInvalidation2014_11_06 =
             CreateInvalidation2014_11_06Response
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateInvalidation2014_11_06Response' <$>
                   x .@? "Invalidation" <*> h .#? "Location")

instance ToElement CreateInvalidation2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}InvalidationBatch"
              .
              _ciInvalidationBatch

instance ToHeaders CreateInvalidation2014_11_06 where
        toHeaders = const mempty

instance ToPath CreateInvalidation2014_11_06 where
        toPath CreateInvalidation2014_11_06'{..}
          = mconcat
              ["/2014-11-06/distribution/",
               toText _ciDistributionId, "/invalidation"]

instance ToQuery CreateInvalidation2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'createInvalidation2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInvalidation'
--
-- * 'cirLocation'
data CreateInvalidation2014_11_06Response = CreateInvalidation2014_11_06Response'{_cirInvalidation :: Maybe Invalidation, _cirLocation :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateInvalidation2014_11_06Response' smart constructor.
createInvalidation2014_11_06Response :: CreateInvalidation2014_11_06Response
createInvalidation2014_11_06Response = CreateInvalidation2014_11_06Response'{_cirInvalidation = Nothing, _cirLocation = Nothing};

-- | The invalidation\'s information.
cirInvalidation :: Lens' CreateInvalidation2014_11_06Response (Maybe Invalidation)
cirInvalidation = lens _cirInvalidation (\ s a -> s{_cirInvalidation = a});

-- | The fully qualified URI of the distribution and invalidation batch
-- request, including the Invalidation ID.
cirLocation :: Lens' CreateInvalidation2014_11_06Response (Maybe Text)
cirLocation = lens _cirLocation (\ s a -> s{_cirLocation = a});

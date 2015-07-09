{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Create a new invalidation.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateInvalidation.html>
module Network.AWS.CloudFront.CreateInvalidation
    (
    -- * Request
      CreateInvalidation
    -- ** Request constructor
    , createInvalidation
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Response
    , CreateInvalidationResponse
    -- ** Response constructor
    , createInvalidationResponse
    -- ** Response lenses
    , cirInvalidation
    , cirLocation
    , cirStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create an invalidation.
--
-- /See:/ 'createInvalidation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciDistributionId'
--
-- * 'ciInvalidationBatch'
data CreateInvalidation = CreateInvalidation'
    { _ciDistributionId    :: !Text
    , _ciInvalidationBatch :: !InvalidationBatch
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInvalidation' smart constructor.
createInvalidation :: Text -> InvalidationBatch -> CreateInvalidation
createInvalidation pDistributionId pInvalidationBatch =
    CreateInvalidation'
    { _ciDistributionId = pDistributionId
    , _ciInvalidationBatch = pInvalidationBatch
    }

-- | The distribution\'s id.
ciDistributionId :: Lens' CreateInvalidation Text
ciDistributionId = lens _ciDistributionId (\ s a -> s{_ciDistributionId = a});

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation InvalidationBatch
ciInvalidationBatch = lens _ciInvalidationBatch (\ s a -> s{_ciInvalidationBatch = a});

instance AWSRequest CreateInvalidation where
        type Sv CreateInvalidation = CloudFront
        type Rs CreateInvalidation =
             CreateInvalidationResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateInvalidationResponse' <$>
                   (parseXML x) <*> (h .#? "Location") <*>
                     (pure (fromEnum s)))

instance ToElement CreateInvalidation where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}InvalidationBatch"
              .
              _ciInvalidationBatch

instance ToHeaders CreateInvalidation where
        toHeaders = const mempty

instance ToPath CreateInvalidation where
        toPath CreateInvalidation'{..}
          = mconcat
              ["/2015-04-17/distribution/",
               toText _ciDistributionId, "/invalidation"]

instance ToQuery CreateInvalidation where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createInvalidationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInvalidation'
--
-- * 'cirLocation'
--
-- * 'cirStatus'
data CreateInvalidationResponse = CreateInvalidationResponse'
    { _cirInvalidation :: !(Maybe Invalidation)
    , _cirLocation     :: !(Maybe Text)
    , _cirStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInvalidationResponse' smart constructor.
createInvalidationResponse :: Int -> CreateInvalidationResponse
createInvalidationResponse pStatus =
    CreateInvalidationResponse'
    { _cirInvalidation = Nothing
    , _cirLocation = Nothing
    , _cirStatus = pStatus
    }

-- | The invalidation\'s information.
cirInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cirInvalidation = lens _cirInvalidation (\ s a -> s{_cirInvalidation = a});

-- | The fully qualified URI of the distribution and invalidation batch
-- request, including the Invalidation ID.
cirLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cirLocation = lens _cirLocation (\ s a -> s{_cirLocation = a});

-- | FIXME: Undocumented member.
cirStatus :: Lens' CreateInvalidationResponse Int
cirStatus = lens _cirStatus (\ s a -> s{_cirStatus = a});

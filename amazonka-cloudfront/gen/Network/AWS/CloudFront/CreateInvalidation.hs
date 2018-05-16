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
-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new invalidation.
--
--
module Network.AWS.CloudFront.CreateInvalidation
    (
    -- * Creating a Request
      createInvalidation
    , CreateInvalidation
    -- * Request Lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Destructuring the Response
    , createInvalidationResponse
    , CreateInvalidationResponse
    -- * Response Lenses
    , cirsInvalidation
    , cirsLocation
    , cirsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to create an invalidation.
--
--
--
-- /See:/ 'createInvalidation' smart constructor.
data CreateInvalidation = CreateInvalidation'
  { _ciDistributionId    :: !Text
  , _ciInvalidationBatch :: !InvalidationBatch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInvalidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciDistributionId' - The distribution's id.
--
-- * 'ciInvalidationBatch' - The batch information for the invalidation.
createInvalidation
    :: Text -- ^ 'ciDistributionId'
    -> InvalidationBatch -- ^ 'ciInvalidationBatch'
    -> CreateInvalidation
createInvalidation pDistributionId_ pInvalidationBatch_ =
  CreateInvalidation'
    { _ciDistributionId = pDistributionId_
    , _ciInvalidationBatch = pInvalidationBatch_
    }


-- | The distribution's id.
ciDistributionId :: Lens' CreateInvalidation Text
ciDistributionId = lens _ciDistributionId (\ s a -> s{_ciDistributionId = a})

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation InvalidationBatch
ciInvalidationBatch = lens _ciInvalidationBatch (\ s a -> s{_ciInvalidationBatch = a})

instance AWSRequest CreateInvalidation where
        type Rs CreateInvalidation =
             CreateInvalidationResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateInvalidationResponse' <$>
                   (parseXML x) <*> (h .#? "Location") <*>
                     (pure (fromEnum s)))

instance Hashable CreateInvalidation where

instance NFData CreateInvalidation where

instance ToElement CreateInvalidation where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}InvalidationBatch"
              .
              _ciInvalidationBatch

instance ToHeaders CreateInvalidation where
        toHeaders = const mempty

instance ToPath CreateInvalidation where
        toPath CreateInvalidation'{..}
          = mconcat
              ["/2017-10-30/distribution/", toBS _ciDistributionId,
               "/invalidation"]

instance ToQuery CreateInvalidation where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'createInvalidationResponse' smart constructor.
data CreateInvalidationResponse = CreateInvalidationResponse'
  { _cirsInvalidation   :: !(Maybe Invalidation)
  , _cirsLocation       :: !(Maybe Text)
  , _cirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInvalidationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsInvalidation' - The invalidation's information.
--
-- * 'cirsLocation' - The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
--
-- * 'cirsResponseStatus' - -- | The response status code.
createInvalidationResponse
    :: Int -- ^ 'cirsResponseStatus'
    -> CreateInvalidationResponse
createInvalidationResponse pResponseStatus_ =
  CreateInvalidationResponse'
    { _cirsInvalidation = Nothing
    , _cirsLocation = Nothing
    , _cirsResponseStatus = pResponseStatus_
    }


-- | The invalidation's information.
cirsInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cirsInvalidation = lens _cirsInvalidation (\ s a -> s{_cirsInvalidation = a})

-- | The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
cirsLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cirsLocation = lens _cirsLocation (\ s a -> s{_cirsLocation = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateInvalidationResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\ s a -> s{_cirsResponseStatus = a})

instance NFData CreateInvalidationResponse where

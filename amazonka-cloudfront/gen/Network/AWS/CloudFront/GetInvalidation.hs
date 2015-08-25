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
-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an invalidation.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetInvalidation.html AWS API Reference> for GetInvalidation.
module Network.AWS.CloudFront.GetInvalidation
    (
    -- * Creating a Request
      getInvalidation
    , GetInvalidation
    -- * Request Lenses
    , giDistributionId
    , giId

    -- * Destructuring the Response
    , getInvalidationResponse
    , GetInvalidationResponse
    -- * Response Lenses
    , girsInvalidation
    , girsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get an invalidation\'s information.
--
-- /See:/ 'getInvalidation' smart constructor.
data GetInvalidation = GetInvalidation'
    { _giDistributionId :: !Text
    , _giId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetInvalidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giDistributionId'
--
-- * 'giId'
getInvalidation
    :: Text -- ^ 'giDistributionId'
    -> Text -- ^ 'giId'
    -> GetInvalidation
getInvalidation pDistributionId_ pId_ =
    GetInvalidation'
    { _giDistributionId = pDistributionId_
    , _giId = pId_
    }

-- | The distribution\'s id.
giDistributionId :: Lens' GetInvalidation Text
giDistributionId = lens _giDistributionId (\ s a -> s{_giDistributionId = a});

-- | The invalidation\'s id.
giId :: Lens' GetInvalidation Text
giId = lens _giId (\ s a -> s{_giId = a});

instance AWSRequest GetInvalidation where
        type Rs GetInvalidation = GetInvalidationResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetInvalidationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance ToHeaders GetInvalidation where
        toHeaders = const mempty

instance ToPath GetInvalidation where
        toPath GetInvalidation'{..}
          = mconcat
              ["/2015-04-17/distribution/", toBS _giDistributionId,
               "/invalidation/", toBS _giId]

instance ToQuery GetInvalidation where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getInvalidationResponse' smart constructor.
data GetInvalidationResponse = GetInvalidationResponse'
    { _girsInvalidation :: !(Maybe Invalidation)
    , _girsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetInvalidationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsInvalidation'
--
-- * 'girsStatus'
getInvalidationResponse
    :: Int -- ^ 'girsStatus'
    -> GetInvalidationResponse
getInvalidationResponse pStatus_ =
    GetInvalidationResponse'
    { _girsInvalidation = Nothing
    , _girsStatus = pStatus_
    }

-- | The invalidation\'s information.
girsInvalidation :: Lens' GetInvalidationResponse (Maybe Invalidation)
girsInvalidation = lens _girsInvalidation (\ s a -> s{_girsInvalidation = a});

-- | The response status code.
girsStatus :: Lens' GetInvalidationResponse Int
girsStatus = lens _girsStatus (\ s a -> s{_girsStatus = a});

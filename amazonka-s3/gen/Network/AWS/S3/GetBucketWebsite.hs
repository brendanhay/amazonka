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
-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the website configuration for a bucket.
module Network.AWS.S3.GetBucketWebsite
    (
    -- * Creating a Request
      getBucketWebsite
    , GetBucketWebsite
    -- * Request Lenses
    , gbwBucket

    -- * Destructuring the Response
    , getBucketWebsiteResponse
    , GetBucketWebsiteResponse
    -- * Response Lenses
    , gbwrsRedirectAllRequestsTo
    , gbwrsErrorDocument
    , gbwrsIndexDocument
    , gbwrsRoutingRules
    , gbwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketWebsite' smart constructor.
newtype GetBucketWebsite = GetBucketWebsite'
  { _gbwBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbwBucket' - Undocumented member.
getBucketWebsite
    :: BucketName -- ^ 'gbwBucket'
    -> GetBucketWebsite
getBucketWebsite pBucket_ = GetBucketWebsite' {_gbwBucket = pBucket_}


-- | Undocumented member.
gbwBucket :: Lens' GetBucketWebsite BucketName
gbwBucket = lens _gbwBucket (\ s a -> s{_gbwBucket = a})

instance AWSRequest GetBucketWebsite where
        type Rs GetBucketWebsite = GetBucketWebsiteResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketWebsiteResponse' <$>
                   (x .@? "RedirectAllRequestsTo") <*>
                     (x .@? "ErrorDocument")
                     <*> (x .@? "IndexDocument")
                     <*>
                     (x .@? "RoutingRules" .!@ mempty >>=
                        may (parseXMLList "RoutingRule"))
                     <*> (pure (fromEnum s)))

instance Hashable GetBucketWebsite where

instance NFData GetBucketWebsite where

instance ToHeaders GetBucketWebsite where
        toHeaders = const mempty

instance ToPath GetBucketWebsite where
        toPath GetBucketWebsite'{..}
          = mconcat ["/", toBS _gbwBucket]

instance ToQuery GetBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'getBucketWebsiteResponse' smart constructor.
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'
  { _gbwrsRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
  , _gbwrsErrorDocument         :: !(Maybe ErrorDocument)
  , _gbwrsIndexDocument         :: !(Maybe IndexDocument)
  , _gbwrsRoutingRules          :: !(Maybe [RoutingRule])
  , _gbwrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketWebsiteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbwrsRedirectAllRequestsTo' - Undocumented member.
--
-- * 'gbwrsErrorDocument' - Undocumented member.
--
-- * 'gbwrsIndexDocument' - Undocumented member.
--
-- * 'gbwrsRoutingRules' - Undocumented member.
--
-- * 'gbwrsResponseStatus' - -- | The response status code.
getBucketWebsiteResponse
    :: Int -- ^ 'gbwrsResponseStatus'
    -> GetBucketWebsiteResponse
getBucketWebsiteResponse pResponseStatus_ =
  GetBucketWebsiteResponse'
    { _gbwrsRedirectAllRequestsTo = Nothing
    , _gbwrsErrorDocument = Nothing
    , _gbwrsIndexDocument = Nothing
    , _gbwrsRoutingRules = Nothing
    , _gbwrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gbwrsRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrsRedirectAllRequestsTo = lens _gbwrsRedirectAllRequestsTo (\ s a -> s{_gbwrsRedirectAllRequestsTo = a})

-- | Undocumented member.
gbwrsErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrsErrorDocument = lens _gbwrsErrorDocument (\ s a -> s{_gbwrsErrorDocument = a})

-- | Undocumented member.
gbwrsIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrsIndexDocument = lens _gbwrsIndexDocument (\ s a -> s{_gbwrsIndexDocument = a})

-- | Undocumented member.
gbwrsRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrsRoutingRules = lens _gbwrsRoutingRules (\ s a -> s{_gbwrsRoutingRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gbwrsResponseStatus :: Lens' GetBucketWebsiteResponse Int
gbwrsResponseStatus = lens _gbwrsResponseStatus (\ s a -> s{_gbwrsResponseStatus = a})

instance NFData GetBucketWebsiteResponse where

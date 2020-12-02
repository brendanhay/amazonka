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
-- Module      : Network.AWS.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool to the configured stream. Customers are limited to one successful bulk publish per 24 hours. Bulk publish is an asynchronous request, customers can see the status of the request via the GetBulkPublishDetails operation.
--
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.BulkPublish
    (
    -- * Creating a Request
      bulkPublish
    , BulkPublish
    -- * Request Lenses
    , bpIdentityPoolId

    -- * Destructuring the Response
    , bulkPublishResponse
    , BulkPublishResponse
    -- * Response Lenses
    , bprsIdentityPoolId
    , bprsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the BulkPublish operation.
--
-- /See:/ 'bulkPublish' smart constructor.
newtype BulkPublish = BulkPublish'
  { _bpIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BulkPublish' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
bulkPublish
    :: Text -- ^ 'bpIdentityPoolId'
    -> BulkPublish
bulkPublish pIdentityPoolId_ =
  BulkPublish' {_bpIdentityPoolId = pIdentityPoolId_}


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
bpIdentityPoolId :: Lens' BulkPublish Text
bpIdentityPoolId = lens _bpIdentityPoolId (\ s a -> s{_bpIdentityPoolId = a})

instance AWSRequest BulkPublish where
        type Rs BulkPublish = BulkPublishResponse
        request = postJSON cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 BulkPublishResponse' <$>
                   (x .?> "IdentityPoolId") <*> (pure (fromEnum s)))

instance Hashable BulkPublish where

instance NFData BulkPublish where

instance ToHeaders BulkPublish where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BulkPublish where
        toJSON = const (Object mempty)

instance ToPath BulkPublish where
        toPath BulkPublish'{..}
          = mconcat
              ["/identitypools/", toBS _bpIdentityPoolId,
               "/bulkpublish"]

instance ToQuery BulkPublish where
        toQuery = const mempty

-- | The output for the BulkPublish operation.
--
-- /See:/ 'bulkPublishResponse' smart constructor.
data BulkPublishResponse = BulkPublishResponse'
  { _bprsIdentityPoolId :: !(Maybe Text)
  , _bprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BulkPublishResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bprsIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'bprsResponseStatus' - -- | The response status code.
bulkPublishResponse
    :: Int -- ^ 'bprsResponseStatus'
    -> BulkPublishResponse
bulkPublishResponse pResponseStatus_ =
  BulkPublishResponse'
    {_bprsIdentityPoolId = Nothing, _bprsResponseStatus = pResponseStatus_}


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
bprsIdentityPoolId :: Lens' BulkPublishResponse (Maybe Text)
bprsIdentityPoolId = lens _bprsIdentityPoolId (\ s a -> s{_bprsIdentityPoolId = a})

-- | -- | The response status code.
bprsResponseStatus :: Lens' BulkPublishResponse Int
bprsResponseStatus = lens _bprsResponseStatus (\ s a -> s{_bprsResponseStatus = a})

instance NFData BulkPublishResponse where

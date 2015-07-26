{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool
-- to the configured stream. Customers are limited to one successful bulk
-- publish per 24 hours. Bulk publish is an asynchronous request, customers
-- can see the status of the request via the GetBulkPublishDetails
-- operation.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_BulkPublish.html>
module Network.AWS.CognitoSync.BulkPublish
    (
    -- * Request
      BulkPublish
    -- ** Request constructor
    , bulkPublish
    -- ** Request lenses
    , bpIdentityPoolId

    -- * Response
    , BulkPublishResponse
    -- ** Response constructor
    , bulkPublishResponse
    -- ** Response lenses
    , bprsIdentityPoolId
    , bprsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the BulkPublish operation.
--
-- /See:/ 'bulkPublish' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bpIdentityPoolId'
newtype BulkPublish = BulkPublish'
    { _bpIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BulkPublish' smart constructor.
bulkPublish :: Text -> BulkPublish
bulkPublish pIdentityPoolId_ =
    BulkPublish'
    { _bpIdentityPoolId = pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bpIdentityPoolId :: Lens' BulkPublish Text
bpIdentityPoolId = lens _bpIdentityPoolId (\ s a -> s{_bpIdentityPoolId = a});

instance AWSRequest BulkPublish where
        type Sv BulkPublish = CognitoSync
        type Rs BulkPublish = BulkPublishResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BulkPublishResponse' <$>
                   (x .?> "IdentityPoolId") <*> (pure (fromEnum s)))

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
              ["/identitypools/", toPath _bpIdentityPoolId,
               "/bulkpublish"]

instance ToQuery BulkPublish where
        toQuery = const mempty

-- | The output for the BulkPublish operation.
--
-- /See:/ 'bulkPublishResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bprsIdentityPoolId'
--
-- * 'bprsStatus'
data BulkPublishResponse = BulkPublishResponse'
    { _bprsIdentityPoolId :: !(Maybe Text)
    , _bprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BulkPublishResponse' smart constructor.
bulkPublishResponse :: Int -> BulkPublishResponse
bulkPublishResponse pStatus_ =
    BulkPublishResponse'
    { _bprsIdentityPoolId = Nothing
    , _bprsStatus = pStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bprsIdentityPoolId :: Lens' BulkPublishResponse (Maybe Text)
bprsIdentityPoolId = lens _bprsIdentityPoolId (\ s a -> s{_bprsIdentityPoolId = a});

-- | FIXME: Undocumented member.
bprsStatus :: Lens' BulkPublishResponse Int
bprsStatus = lens _bprsStatus (\ s a -> s{_bprsStatus = a});

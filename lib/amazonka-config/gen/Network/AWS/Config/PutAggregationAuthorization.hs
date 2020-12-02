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
-- Module      : Network.AWS.Config.PutAggregationAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the aggregator account and region to collect data from the source account and region.
--
--
module Network.AWS.Config.PutAggregationAuthorization
    (
    -- * Creating a Request
      putAggregationAuthorization
    , PutAggregationAuthorization
    -- * Request Lenses
    , paaAuthorizedAccountId
    , paaAuthorizedAWSRegion

    -- * Destructuring the Response
    , putAggregationAuthorizationResponse
    , PutAggregationAuthorizationResponse
    -- * Response Lenses
    , paarsAggregationAuthorization
    , paarsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAggregationAuthorization' smart constructor.
data PutAggregationAuthorization = PutAggregationAuthorization'
  { _paaAuthorizedAccountId :: !Text
  , _paaAuthorizedAWSRegion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAggregationAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paaAuthorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- * 'paaAuthorizedAWSRegion' - The region authorized to collect aggregated data.
putAggregationAuthorization
    :: Text -- ^ 'paaAuthorizedAccountId'
    -> Text -- ^ 'paaAuthorizedAWSRegion'
    -> PutAggregationAuthorization
putAggregationAuthorization pAuthorizedAccountId_ pAuthorizedAWSRegion_ =
  PutAggregationAuthorization'
    { _paaAuthorizedAccountId = pAuthorizedAccountId_
    , _paaAuthorizedAWSRegion = pAuthorizedAWSRegion_
    }


-- | The 12-digit account ID of the account authorized to aggregate data.
paaAuthorizedAccountId :: Lens' PutAggregationAuthorization Text
paaAuthorizedAccountId = lens _paaAuthorizedAccountId (\ s a -> s{_paaAuthorizedAccountId = a})

-- | The region authorized to collect aggregated data.
paaAuthorizedAWSRegion :: Lens' PutAggregationAuthorization Text
paaAuthorizedAWSRegion = lens _paaAuthorizedAWSRegion (\ s a -> s{_paaAuthorizedAWSRegion = a})

instance AWSRequest PutAggregationAuthorization where
        type Rs PutAggregationAuthorization =
             PutAggregationAuthorizationResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 PutAggregationAuthorizationResponse' <$>
                   (x .?> "AggregationAuthorization") <*>
                     (pure (fromEnum s)))

instance Hashable PutAggregationAuthorization where

instance NFData PutAggregationAuthorization where

instance ToHeaders PutAggregationAuthorization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutAggregationAuthorization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAggregationAuthorization where
        toJSON PutAggregationAuthorization'{..}
          = object
              (catMaybes
                 [Just
                    ("AuthorizedAccountId" .= _paaAuthorizedAccountId),
                  Just
                    ("AuthorizedAwsRegion" .= _paaAuthorizedAWSRegion)])

instance ToPath PutAggregationAuthorization where
        toPath = const "/"

instance ToQuery PutAggregationAuthorization where
        toQuery = const mempty

-- | /See:/ 'putAggregationAuthorizationResponse' smart constructor.
data PutAggregationAuthorizationResponse = PutAggregationAuthorizationResponse'
  { _paarsAggregationAuthorization :: !(Maybe AggregationAuthorization)
  , _paarsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAggregationAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paarsAggregationAuthorization' - Returns an AggregationAuthorization object.
--
-- * 'paarsResponseStatus' - -- | The response status code.
putAggregationAuthorizationResponse
    :: Int -- ^ 'paarsResponseStatus'
    -> PutAggregationAuthorizationResponse
putAggregationAuthorizationResponse pResponseStatus_ =
  PutAggregationAuthorizationResponse'
    { _paarsAggregationAuthorization = Nothing
    , _paarsResponseStatus = pResponseStatus_
    }


-- | Returns an AggregationAuthorization object.
paarsAggregationAuthorization :: Lens' PutAggregationAuthorizationResponse (Maybe AggregationAuthorization)
paarsAggregationAuthorization = lens _paarsAggregationAuthorization (\ s a -> s{_paarsAggregationAuthorization = a})

-- | -- | The response status code.
paarsResponseStatus :: Lens' PutAggregationAuthorizationResponse Int
paarsResponseStatus = lens _paarsResponseStatus (\ s a -> s{_paarsResponseStatus = a})

instance NFData PutAggregationAuthorizationResponse
         where

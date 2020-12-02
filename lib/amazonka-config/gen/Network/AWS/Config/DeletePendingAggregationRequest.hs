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
-- Module      : Network.AWS.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator account in a specified region.
--
--
module Network.AWS.Config.DeletePendingAggregationRequest
    (
    -- * Creating a Request
      deletePendingAggregationRequest
    , DeletePendingAggregationRequest
    -- * Request Lenses
    , dparRequesterAccountId
    , dparRequesterAWSRegion

    -- * Destructuring the Response
    , deletePendingAggregationRequestResponse
    , DeletePendingAggregationRequestResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { _dparRequesterAccountId :: !Text
  , _dparRequesterAWSRegion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePendingAggregationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dparRequesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
--
-- * 'dparRequesterAWSRegion' - The region requesting to aggregate data.
deletePendingAggregationRequest
    :: Text -- ^ 'dparRequesterAccountId'
    -> Text -- ^ 'dparRequesterAWSRegion'
    -> DeletePendingAggregationRequest
deletePendingAggregationRequest pRequesterAccountId_ pRequesterAWSRegion_ =
  DeletePendingAggregationRequest'
    { _dparRequesterAccountId = pRequesterAccountId_
    , _dparRequesterAWSRegion = pRequesterAWSRegion_
    }


-- | The 12-digit account ID of the account requesting to aggregate data.
dparRequesterAccountId :: Lens' DeletePendingAggregationRequest Text
dparRequesterAccountId = lens _dparRequesterAccountId (\ s a -> s{_dparRequesterAccountId = a})

-- | The region requesting to aggregate data.
dparRequesterAWSRegion :: Lens' DeletePendingAggregationRequest Text
dparRequesterAWSRegion = lens _dparRequesterAWSRegion (\ s a -> s{_dparRequesterAWSRegion = a})

instance AWSRequest DeletePendingAggregationRequest
         where
        type Rs DeletePendingAggregationRequest =
             DeletePendingAggregationRequestResponse
        request = postJSON config
        response
          = receiveNull
              DeletePendingAggregationRequestResponse'

instance Hashable DeletePendingAggregationRequest
         where

instance NFData DeletePendingAggregationRequest where

instance ToHeaders DeletePendingAggregationRequest
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeletePendingAggregationRequest"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePendingAggregationRequest where
        toJSON DeletePendingAggregationRequest'{..}
          = object
              (catMaybes
                 [Just
                    ("RequesterAccountId" .= _dparRequesterAccountId),
                  Just
                    ("RequesterAwsRegion" .= _dparRequesterAWSRegion)])

instance ToPath DeletePendingAggregationRequest where
        toPath = const "/"

instance ToQuery DeletePendingAggregationRequest
         where
        toQuery = const mempty

-- | /See:/ 'deletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePendingAggregationRequestResponse' with the minimum fields required to make a request.
--
deletePendingAggregationRequestResponse
    :: DeletePendingAggregationRequestResponse
deletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'


instance NFData
           DeletePendingAggregationRequestResponse
         where

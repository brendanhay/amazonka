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
-- Module      : Network.AWS.Config.DeleteAggregationAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the authorization granted to the specified configuration aggregator account in a specified region.
--
--
module Network.AWS.Config.DeleteAggregationAuthorization
    (
    -- * Creating a Request
      deleteAggregationAuthorization
    , DeleteAggregationAuthorization
    -- * Request Lenses
    , daaAuthorizedAccountId
    , daaAuthorizedAWSRegion

    -- * Destructuring the Response
    , deleteAggregationAuthorizationResponse
    , DeleteAggregationAuthorizationResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAggregationAuthorization' smart constructor.
data DeleteAggregationAuthorization = DeleteAggregationAuthorization'
  { _daaAuthorizedAccountId :: !Text
  , _daaAuthorizedAWSRegion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAggregationAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaAuthorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- * 'daaAuthorizedAWSRegion' - The region authorized to collect aggregated data.
deleteAggregationAuthorization
    :: Text -- ^ 'daaAuthorizedAccountId'
    -> Text -- ^ 'daaAuthorizedAWSRegion'
    -> DeleteAggregationAuthorization
deleteAggregationAuthorization pAuthorizedAccountId_ pAuthorizedAWSRegion_ =
  DeleteAggregationAuthorization'
    { _daaAuthorizedAccountId = pAuthorizedAccountId_
    , _daaAuthorizedAWSRegion = pAuthorizedAWSRegion_
    }


-- | The 12-digit account ID of the account authorized to aggregate data.
daaAuthorizedAccountId :: Lens' DeleteAggregationAuthorization Text
daaAuthorizedAccountId = lens _daaAuthorizedAccountId (\ s a -> s{_daaAuthorizedAccountId = a})

-- | The region authorized to collect aggregated data.
daaAuthorizedAWSRegion :: Lens' DeleteAggregationAuthorization Text
daaAuthorizedAWSRegion = lens _daaAuthorizedAWSRegion (\ s a -> s{_daaAuthorizedAWSRegion = a})

instance AWSRequest DeleteAggregationAuthorization
         where
        type Rs DeleteAggregationAuthorization =
             DeleteAggregationAuthorizationResponse
        request = postJSON config
        response
          = receiveNull DeleteAggregationAuthorizationResponse'

instance Hashable DeleteAggregationAuthorization
         where

instance NFData DeleteAggregationAuthorization where

instance ToHeaders DeleteAggregationAuthorization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteAggregationAuthorization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAggregationAuthorization where
        toJSON DeleteAggregationAuthorization'{..}
          = object
              (catMaybes
                 [Just
                    ("AuthorizedAccountId" .= _daaAuthorizedAccountId),
                  Just
                    ("AuthorizedAwsRegion" .= _daaAuthorizedAWSRegion)])

instance ToPath DeleteAggregationAuthorization where
        toPath = const "/"

instance ToQuery DeleteAggregationAuthorization where
        toQuery = const mempty

-- | /See:/ 'deleteAggregationAuthorizationResponse' smart constructor.
data DeleteAggregationAuthorizationResponse =
  DeleteAggregationAuthorizationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAggregationAuthorizationResponse' with the minimum fields required to make a request.
--
deleteAggregationAuthorizationResponse
    :: DeleteAggregationAuthorizationResponse
deleteAggregationAuthorizationResponse = DeleteAggregationAuthorizationResponse'


instance NFData
           DeleteAggregationAuthorizationResponse
         where

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
-- Module      : Network.AWS.SSM.GetInventorySchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type.
--
--
module Network.AWS.SSM.GetInventorySchema
    (
    -- * Creating a Request
      getInventorySchema
    , GetInventorySchema
    -- * Request Lenses
    , gisTypeName
    , gisAggregator
    , gisNextToken
    , gisSubType
    , gisMaxResults

    -- * Destructuring the Response
    , getInventorySchemaResponse
    , GetInventorySchemaResponse
    -- * Response Lenses
    , gisrsSchemas
    , gisrsNextToken
    , gisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { _gisTypeName   :: !(Maybe Text)
  , _gisAggregator :: !(Maybe Bool)
  , _gisNextToken  :: !(Maybe Text)
  , _gisSubType    :: !(Maybe Bool)
  , _gisMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInventorySchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisTypeName' - The type of inventory item to return.
--
-- * 'gisAggregator' - Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
--
-- * 'gisNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'gisSubType' - Returns the sub-type schema for a specified inventory type.
--
-- * 'gisMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
getInventorySchema
    :: GetInventorySchema
getInventorySchema =
  GetInventorySchema'
    { _gisTypeName = Nothing
    , _gisAggregator = Nothing
    , _gisNextToken = Nothing
    , _gisSubType = Nothing
    , _gisMaxResults = Nothing
    }


-- | The type of inventory item to return.
gisTypeName :: Lens' GetInventorySchema (Maybe Text)
gisTypeName = lens _gisTypeName (\ s a -> s{_gisTypeName = a})

-- | Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
gisAggregator :: Lens' GetInventorySchema (Maybe Bool)
gisAggregator = lens _gisAggregator (\ s a -> s{_gisAggregator = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
gisNextToken :: Lens' GetInventorySchema (Maybe Text)
gisNextToken = lens _gisNextToken (\ s a -> s{_gisNextToken = a})

-- | Returns the sub-type schema for a specified inventory type.
gisSubType :: Lens' GetInventorySchema (Maybe Bool)
gisSubType = lens _gisSubType (\ s a -> s{_gisSubType = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
gisMaxResults :: Lens' GetInventorySchema (Maybe Natural)
gisMaxResults = lens _gisMaxResults (\ s a -> s{_gisMaxResults = a}) . mapping _Nat

instance AWSRequest GetInventorySchema where
        type Rs GetInventorySchema =
             GetInventorySchemaResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetInventorySchemaResponse' <$>
                   (x .?> "Schemas" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetInventorySchema where

instance NFData GetInventorySchema where

instance ToHeaders GetInventorySchema where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetInventorySchema" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInventorySchema where
        toJSON GetInventorySchema'{..}
          = object
              (catMaybes
                 [("TypeName" .=) <$> _gisTypeName,
                  ("Aggregator" .=) <$> _gisAggregator,
                  ("NextToken" .=) <$> _gisNextToken,
                  ("SubType" .=) <$> _gisSubType,
                  ("MaxResults" .=) <$> _gisMaxResults])

instance ToPath GetInventorySchema where
        toPath = const "/"

instance ToQuery GetInventorySchema where
        toQuery = const mempty

-- | /See:/ 'getInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { _gisrsSchemas        :: !(Maybe [InventoryItemSchema])
  , _gisrsNextToken      :: !(Maybe Text)
  , _gisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInventorySchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsSchemas' - Inventory schemas returned by the request.
--
-- * 'gisrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'gisrsResponseStatus' - -- | The response status code.
getInventorySchemaResponse
    :: Int -- ^ 'gisrsResponseStatus'
    -> GetInventorySchemaResponse
getInventorySchemaResponse pResponseStatus_ =
  GetInventorySchemaResponse'
    { _gisrsSchemas = Nothing
    , _gisrsNextToken = Nothing
    , _gisrsResponseStatus = pResponseStatus_
    }


-- | Inventory schemas returned by the request.
gisrsSchemas :: Lens' GetInventorySchemaResponse [InventoryItemSchema]
gisrsSchemas = lens _gisrsSchemas (\ s a -> s{_gisrsSchemas = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
gisrsNextToken :: Lens' GetInventorySchemaResponse (Maybe Text)
gisrsNextToken = lens _gisrsNextToken (\ s a -> s{_gisrsNextToken = a})

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetInventorySchemaResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\ s a -> s{_gisrsResponseStatus = a})

instance NFData GetInventorySchemaResponse where

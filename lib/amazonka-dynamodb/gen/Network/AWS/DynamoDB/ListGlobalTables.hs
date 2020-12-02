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
-- Module      : Network.AWS.DynamoDB.ListGlobalTables
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all global tables that have a replica in the specified region.
--
--
module Network.AWS.DynamoDB.ListGlobalTables
    (
    -- * Creating a Request
      listGlobalTables
    , ListGlobalTables
    -- * Request Lenses
    , lgtRegionName
    , lgtExclusiveStartGlobalTableName
    , lgtLimit

    -- * Destructuring the Response
    , listGlobalTablesResponse
    , ListGlobalTablesResponse
    -- * Response Lenses
    , lgtrsLastEvaluatedGlobalTableName
    , lgtrsGlobalTables
    , lgtrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { _lgtRegionName                    :: !(Maybe Text)
  , _lgtExclusiveStartGlobalTableName :: !(Maybe Text)
  , _lgtLimit                         :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGlobalTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgtRegionName' - Lists the global tables in a specific region.
--
-- * 'lgtExclusiveStartGlobalTableName' - The first global table name that this operation will evaluate.
--
-- * 'lgtLimit' - The maximum number of table names to return.
listGlobalTables
    :: ListGlobalTables
listGlobalTables =
  ListGlobalTables'
    { _lgtRegionName = Nothing
    , _lgtExclusiveStartGlobalTableName = Nothing
    , _lgtLimit = Nothing
    }


-- | Lists the global tables in a specific region.
lgtRegionName :: Lens' ListGlobalTables (Maybe Text)
lgtRegionName = lens _lgtRegionName (\ s a -> s{_lgtRegionName = a})

-- | The first global table name that this operation will evaluate.
lgtExclusiveStartGlobalTableName :: Lens' ListGlobalTables (Maybe Text)
lgtExclusiveStartGlobalTableName = lens _lgtExclusiveStartGlobalTableName (\ s a -> s{_lgtExclusiveStartGlobalTableName = a})

-- | The maximum number of table names to return.
lgtLimit :: Lens' ListGlobalTables (Maybe Natural)
lgtLimit = lens _lgtLimit (\ s a -> s{_lgtLimit = a}) . mapping _Nat

instance AWSRequest ListGlobalTables where
        type Rs ListGlobalTables = ListGlobalTablesResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 ListGlobalTablesResponse' <$>
                   (x .?> "LastEvaluatedGlobalTableName") <*>
                     (x .?> "GlobalTables" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListGlobalTables where

instance NFData ListGlobalTables where

instance ToHeaders ListGlobalTables where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.ListGlobalTables" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListGlobalTables where
        toJSON ListGlobalTables'{..}
          = object
              (catMaybes
                 [("RegionName" .=) <$> _lgtRegionName,
                  ("ExclusiveStartGlobalTableName" .=) <$>
                    _lgtExclusiveStartGlobalTableName,
                  ("Limit" .=) <$> _lgtLimit])

instance ToPath ListGlobalTables where
        toPath = const "/"

instance ToQuery ListGlobalTables where
        toQuery = const mempty

-- | /See:/ 'listGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { _lgtrsLastEvaluatedGlobalTableName :: !(Maybe Text)
  , _lgtrsGlobalTables                 :: !(Maybe [GlobalTable])
  , _lgtrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGlobalTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgtrsLastEvaluatedGlobalTableName' - Last evaluated global table name.
--
-- * 'lgtrsGlobalTables' - List of global table names.
--
-- * 'lgtrsResponseStatus' - -- | The response status code.
listGlobalTablesResponse
    :: Int -- ^ 'lgtrsResponseStatus'
    -> ListGlobalTablesResponse
listGlobalTablesResponse pResponseStatus_ =
  ListGlobalTablesResponse'
    { _lgtrsLastEvaluatedGlobalTableName = Nothing
    , _lgtrsGlobalTables = Nothing
    , _lgtrsResponseStatus = pResponseStatus_
    }


-- | Last evaluated global table name.
lgtrsLastEvaluatedGlobalTableName :: Lens' ListGlobalTablesResponse (Maybe Text)
lgtrsLastEvaluatedGlobalTableName = lens _lgtrsLastEvaluatedGlobalTableName (\ s a -> s{_lgtrsLastEvaluatedGlobalTableName = a})

-- | List of global table names.
lgtrsGlobalTables :: Lens' ListGlobalTablesResponse [GlobalTable]
lgtrsGlobalTables = lens _lgtrsGlobalTables (\ s a -> s{_lgtrsGlobalTables = a}) . _Default . _Coerce

-- | -- | The response status code.
lgtrsResponseStatus :: Lens' ListGlobalTablesResponse Int
lgtrsResponseStatus = lens _lgtrsResponseStatus (\ s a -> s{_lgtrsResponseStatus = a})

instance NFData ListGlobalTablesResponse where

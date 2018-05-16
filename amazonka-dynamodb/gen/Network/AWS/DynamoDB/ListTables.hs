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
-- Module      : Network.AWS.DynamoDB.ListTables
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of table names associated with the current account and endpoint. The output from @ListTables@ is paginated, with each page returning a maximum of 100 table names.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListTables
    (
    -- * Creating a Request
      listTables
    , ListTables
    -- * Request Lenses
    , ltExclusiveStartTableName
    , ltLimit

    -- * Destructuring the Response
    , listTablesResponse
    , ListTablesResponse
    -- * Response Lenses
    , ltrsLastEvaluatedTableName
    , ltrsTableNames
    , ltrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @ListTables@ operation.
--
--
--
-- /See:/ 'listTables' smart constructor.
data ListTables = ListTables'
  { _ltExclusiveStartTableName :: !(Maybe Text)
  , _ltLimit                   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltExclusiveStartTableName' - The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
--
-- * 'ltLimit' - A maximum number of table names to return. If this parameter is not specified, the limit is 100.
listTables
    :: ListTables
listTables =
  ListTables' {_ltExclusiveStartTableName = Nothing, _ltLimit = Nothing}


-- | The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
ltExclusiveStartTableName :: Lens' ListTables (Maybe Text)
ltExclusiveStartTableName = lens _ltExclusiveStartTableName (\ s a -> s{_ltExclusiveStartTableName = a})

-- | A maximum number of table names to return. If this parameter is not specified, the limit is 100.
ltLimit :: Lens' ListTables (Maybe Natural)
ltLimit = lens _ltLimit (\ s a -> s{_ltLimit = a}) . mapping _Nat

instance AWSPager ListTables where
        page rq rs
          | stop (rs ^. ltrsLastEvaluatedTableName) = Nothing
          | stop (rs ^. ltrsTableNames) = Nothing
          | otherwise =
            Just $ rq &
              ltExclusiveStartTableName .~
                rs ^. ltrsLastEvaluatedTableName

instance AWSRequest ListTables where
        type Rs ListTables = ListTablesResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 ListTablesResponse' <$>
                   (x .?> "LastEvaluatedTableName") <*>
                     (x .?> "TableNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTables where

instance NFData ListTables where

instance ToHeaders ListTables where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.ListTables" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListTables where
        toJSON ListTables'{..}
          = object
              (catMaybes
                 [("ExclusiveStartTableName" .=) <$>
                    _ltExclusiveStartTableName,
                  ("Limit" .=) <$> _ltLimit])

instance ToPath ListTables where
        toPath = const "/"

instance ToQuery ListTables where
        toQuery = const mempty

-- | Represents the output of a @ListTables@ operation.
--
--
--
-- /See:/ 'listTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { _ltrsLastEvaluatedTableName :: !(Maybe Text)
  , _ltrsTableNames             :: !(Maybe [Text])
  , _ltrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsLastEvaluatedTableName' - The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned. If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
--
-- * 'ltrsTableNames' - The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100. If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTablesResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTablesResponse
listTablesResponse pResponseStatus_ =
  ListTablesResponse'
    { _ltrsLastEvaluatedTableName = Nothing
    , _ltrsTableNames = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned. If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
ltrsLastEvaluatedTableName :: Lens' ListTablesResponse (Maybe Text)
ltrsLastEvaluatedTableName = lens _ltrsLastEvaluatedTableName (\ s a -> s{_ltrsLastEvaluatedTableName = a})

-- | The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100. If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
ltrsTableNames :: Lens' ListTablesResponse [Text]
ltrsTableNames = lens _ltrsTableNames (\ s a -> s{_ltrsTableNames = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTablesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTablesResponse where

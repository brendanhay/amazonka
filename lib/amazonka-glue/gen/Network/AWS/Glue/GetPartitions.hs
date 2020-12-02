{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the partitions in a table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitions
  ( -- * Creating a Request
    getPartitions,
    GetPartitions,

    -- * Request Lenses
    gpsCatalogId,
    gpsNextToken,
    gpsExpression,
    gpsSegment,
    gpsMaxResults,
    gpsDatabaseName,
    gpsTableName,

    -- * Destructuring the Response
    getPartitionsResponse,
    GetPartitionsResponse,

    -- * Response Lenses
    gpsrsPartitions,
    gpsrsNextToken,
    gpsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPartitions' smart constructor.
data GetPartitions = GetPartitions'
  { _gpsCatalogId :: !(Maybe Text),
    _gpsNextToken :: !(Maybe Text),
    _gpsExpression :: !(Maybe Text),
    _gpsSegment :: !(Maybe Segment),
    _gpsMaxResults :: !(Maybe Nat),
    _gpsDatabaseName :: !Text,
    _gpsTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPartitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpsCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
--
-- * 'gpsNextToken' - A continuation token, if this is not the first call to retrieve these partitions.
--
-- * 'gpsExpression' - An expression that filters the partitions to be returned. The expression uses SQL syntax similar to the SQL @WHERE@ filter clause. The SQL statement parser <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the expression.  /Operators/ : The following are the operators that you can use in the @Expression@ API call:     * =    * Checks whether the values of the two operands are equal; if yes, then the condition becomes true. Example: Assume 'variable a' holds 10 and 'variable b' holds 20.  (a = b) is not true.     * < >    * Checks whether the values of two operands are equal; if the values are not equal, then the condition becomes true. Example: (a < > b) is true.     * >    * Checks whether the value of the left operand is greater than the value of the right operand; if yes, then the condition becomes true. Example: (a > b) is not true.     * <    * Checks whether the value of the left operand is less than the value of the right operand; if yes, then the condition becomes true. Example: (a < b) is true.     * >=    * Checks whether the value of the left operand is greater than or equal to the value of the right operand; if yes, then the condition becomes true. Example: (a >= b) is not true.     * <=    * Checks whether the value of the left operand is less than or equal to the value of the right operand; if yes, then the condition becomes true. Example: (a <= b) is true.     * AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL    * Logical operators. /Supported Partition Key Types/ : The following are the supported partition keys.     * @string@      * @date@      * @timestamp@      * @int@      * @bigint@      * @long@      * @tinyint@      * @smallint@      * @decimal@  If an invalid type is encountered, an exception is thrown.  The following list shows the valid operators on each type. When you define a crawler, the @partitionKey@ type is created as a @STRING@ , to be compatible with the catalog partitions.  /Sample API Call/ :
--
-- * 'gpsSegment' - The segment of the table's partitions to scan in this request.
--
-- * 'gpsMaxResults' - The maximum number of partitions to return in a single response.
--
-- * 'gpsDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'gpsTableName' - The name of the partitions' table.
getPartitions ::
  -- | 'gpsDatabaseName'
  Text ->
  -- | 'gpsTableName'
  Text ->
  GetPartitions
getPartitions pDatabaseName_ pTableName_ =
  GetPartitions'
    { _gpsCatalogId = Nothing,
      _gpsNextToken = Nothing,
      _gpsExpression = Nothing,
      _gpsSegment = Nothing,
      _gpsMaxResults = Nothing,
      _gpsDatabaseName = pDatabaseName_,
      _gpsTableName = pTableName_
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is provided, the AWS account ID is used by default.
gpsCatalogId :: Lens' GetPartitions (Maybe Text)
gpsCatalogId = lens _gpsCatalogId (\s a -> s {_gpsCatalogId = a})

-- | A continuation token, if this is not the first call to retrieve these partitions.
gpsNextToken :: Lens' GetPartitions (Maybe Text)
gpsNextToken = lens _gpsNextToken (\s a -> s {_gpsNextToken = a})

-- | An expression that filters the partitions to be returned. The expression uses SQL syntax similar to the SQL @WHERE@ filter clause. The SQL statement parser <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the expression.  /Operators/ : The following are the operators that you can use in the @Expression@ API call:     * =    * Checks whether the values of the two operands are equal; if yes, then the condition becomes true. Example: Assume 'variable a' holds 10 and 'variable b' holds 20.  (a = b) is not true.     * < >    * Checks whether the values of two operands are equal; if the values are not equal, then the condition becomes true. Example: (a < > b) is true.     * >    * Checks whether the value of the left operand is greater than the value of the right operand; if yes, then the condition becomes true. Example: (a > b) is not true.     * <    * Checks whether the value of the left operand is less than the value of the right operand; if yes, then the condition becomes true. Example: (a < b) is true.     * >=    * Checks whether the value of the left operand is greater than or equal to the value of the right operand; if yes, then the condition becomes true. Example: (a >= b) is not true.     * <=    * Checks whether the value of the left operand is less than or equal to the value of the right operand; if yes, then the condition becomes true. Example: (a <= b) is true.     * AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL    * Logical operators. /Supported Partition Key Types/ : The following are the supported partition keys.     * @string@      * @date@      * @timestamp@      * @int@      * @bigint@      * @long@      * @tinyint@      * @smallint@      * @decimal@  If an invalid type is encountered, an exception is thrown.  The following list shows the valid operators on each type. When you define a crawler, the @partitionKey@ type is created as a @STRING@ , to be compatible with the catalog partitions.  /Sample API Call/ :
gpsExpression :: Lens' GetPartitions (Maybe Text)
gpsExpression = lens _gpsExpression (\s a -> s {_gpsExpression = a})

-- | The segment of the table's partitions to scan in this request.
gpsSegment :: Lens' GetPartitions (Maybe Segment)
gpsSegment = lens _gpsSegment (\s a -> s {_gpsSegment = a})

-- | The maximum number of partitions to return in a single response.
gpsMaxResults :: Lens' GetPartitions (Maybe Natural)
gpsMaxResults = lens _gpsMaxResults (\s a -> s {_gpsMaxResults = a}) . mapping _Nat

-- | The name of the catalog database where the partitions reside.
gpsDatabaseName :: Lens' GetPartitions Text
gpsDatabaseName = lens _gpsDatabaseName (\s a -> s {_gpsDatabaseName = a})

-- | The name of the partitions' table.
gpsTableName :: Lens' GetPartitions Text
gpsTableName = lens _gpsTableName (\s a -> s {_gpsTableName = a})

instance AWSPager GetPartitions where
  page rq rs
    | stop (rs ^. gpsrsNextToken) = Nothing
    | stop (rs ^. gpsrsPartitions) = Nothing
    | otherwise = Just $ rq & gpsNextToken .~ rs ^. gpsrsNextToken

instance AWSRequest GetPartitions where
  type Rs GetPartitions = GetPartitionsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetPartitionsResponse'
            <$> (x .?> "Partitions" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetPartitions

instance NFData GetPartitions

instance ToHeaders GetPartitions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetPartitions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPartitions where
  toJSON GetPartitions' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gpsCatalogId,
            ("NextToken" .=) <$> _gpsNextToken,
            ("Expression" .=) <$> _gpsExpression,
            ("Segment" .=) <$> _gpsSegment,
            ("MaxResults" .=) <$> _gpsMaxResults,
            Just ("DatabaseName" .= _gpsDatabaseName),
            Just ("TableName" .= _gpsTableName)
          ]
      )

instance ToPath GetPartitions where
  toPath = const "/"

instance ToQuery GetPartitions where
  toQuery = const mempty

-- | /See:/ 'getPartitionsResponse' smart constructor.
data GetPartitionsResponse = GetPartitionsResponse'
  { _gpsrsPartitions ::
      !(Maybe [Partition]),
    _gpsrsNextToken :: !(Maybe Text),
    _gpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPartitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpsrsPartitions' - A list of requested partitions.
--
-- * 'gpsrsNextToken' - A continuation token, if the returned list of partitions does not include the last one.
--
-- * 'gpsrsResponseStatus' - -- | The response status code.
getPartitionsResponse ::
  -- | 'gpsrsResponseStatus'
  Int ->
  GetPartitionsResponse
getPartitionsResponse pResponseStatus_ =
  GetPartitionsResponse'
    { _gpsrsPartitions = Nothing,
      _gpsrsNextToken = Nothing,
      _gpsrsResponseStatus = pResponseStatus_
    }

-- | A list of requested partitions.
gpsrsPartitions :: Lens' GetPartitionsResponse [Partition]
gpsrsPartitions = lens _gpsrsPartitions (\s a -> s {_gpsrsPartitions = a}) . _Default . _Coerce

-- | A continuation token, if the returned list of partitions does not include the last one.
gpsrsNextToken :: Lens' GetPartitionsResponse (Maybe Text)
gpsrsNextToken = lens _gpsrsNextToken (\s a -> s {_gpsrsNextToken = a})

-- | -- | The response status code.
gpsrsResponseStatus :: Lens' GetPartitionsResponse Int
gpsrsResponseStatus = lens _gpsrsResponseStatus (\s a -> s {_gpsrsResponseStatus = a})

instance NFData GetPartitionsResponse

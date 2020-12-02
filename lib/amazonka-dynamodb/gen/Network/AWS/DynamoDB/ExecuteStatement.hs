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
-- Module      : Network.AWS.DynamoDB.ExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform reads and singleton writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.ExecuteStatement
  ( -- * Creating a Request
    executeStatement,
    ExecuteStatement,

    -- * Request Lenses
    esConsistentRead,
    esNextToken,
    esParameters,
    esStatement,

    -- * Destructuring the Response
    executeStatementResponse,
    ExecuteStatementResponse,

    -- * Response Lenses
    esrsItems,
    esrsNextToken,
    esrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'executeStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { _esConsistentRead ::
      !(Maybe Bool),
    _esNextToken :: !(Maybe Text),
    _esParameters :: !(Maybe (List1 AttributeValue)),
    _esStatement :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteStatement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esConsistentRead' - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- * 'esNextToken' - Set this value to get remaining results, if @NextToken@ was returned in the statement response.
--
-- * 'esParameters' - The parameters for the PartiQL statement, if any.
--
-- * 'esStatement' - The PartiQL statement representing the operation to run.
executeStatement ::
  -- | 'esStatement'
  Text ->
  ExecuteStatement
executeStatement pStatement_ =
  ExecuteStatement'
    { _esConsistentRead = Nothing,
      _esNextToken = Nothing,
      _esParameters = Nothing,
      _esStatement = pStatement_
    }

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
esConsistentRead :: Lens' ExecuteStatement (Maybe Bool)
esConsistentRead = lens _esConsistentRead (\s a -> s {_esConsistentRead = a})

-- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
esNextToken :: Lens' ExecuteStatement (Maybe Text)
esNextToken = lens _esNextToken (\s a -> s {_esNextToken = a})

-- | The parameters for the PartiQL statement, if any.
esParameters :: Lens' ExecuteStatement (Maybe (NonEmpty AttributeValue))
esParameters = lens _esParameters (\s a -> s {_esParameters = a}) . mapping _List1

-- | The PartiQL statement representing the operation to run.
esStatement :: Lens' ExecuteStatement Text
esStatement = lens _esStatement (\s a -> s {_esStatement = a})

instance AWSRequest ExecuteStatement where
  type Rs ExecuteStatement = ExecuteStatementResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            <$> (x .?> "Items" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ExecuteStatement

instance NFData ExecuteStatement

instance ToHeaders ExecuteStatement where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.ExecuteStatement" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    object
      ( catMaybes
          [ ("ConsistentRead" .=) <$> _esConsistentRead,
            ("NextToken" .=) <$> _esNextToken,
            ("Parameters" .=) <$> _esParameters,
            Just ("Statement" .= _esStatement)
          ]
      )

instance ToPath ExecuteStatement where
  toPath = const "/"

instance ToQuery ExecuteStatement where
  toQuery = const mempty

-- | /See:/ 'executeStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { _esrsItems ::
      !(Maybe [Map Text (AttributeValue)]),
    _esrsNextToken :: !(Maybe Text),
    _esrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteStatementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsItems' - If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
--
-- * 'esrsNextToken' - If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
--
-- * 'esrsResponseStatus' - -- | The response status code.
executeStatementResponse ::
  -- | 'esrsResponseStatus'
  Int ->
  ExecuteStatementResponse
executeStatementResponse pResponseStatus_ =
  ExecuteStatementResponse'
    { _esrsItems = Nothing,
      _esrsNextToken = Nothing,
      _esrsResponseStatus = pResponseStatus_
    }

-- | If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
esrsItems :: Lens' ExecuteStatementResponse [HashMap Text (AttributeValue)]
esrsItems = lens _esrsItems (\s a -> s {_esrsItems = a}) . _Default . _Coerce

-- | If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
esrsNextToken :: Lens' ExecuteStatementResponse (Maybe Text)
esrsNextToken = lens _esrsNextToken (\s a -> s {_esrsNextToken = a})

-- | -- | The response status code.
esrsResponseStatus :: Lens' ExecuteStatementResponse Int
esrsResponseStatus = lens _esrsResponseStatus (\s a -> s {_esrsResponseStatus = a})

instance NFData ExecuteStatementResponse

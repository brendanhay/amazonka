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
-- Module      : Network.AWS.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.BatchExecuteStatement
  ( -- * Creating a Request
    batchExecuteStatement,
    BatchExecuteStatement,

    -- * Request Lenses
    besStatements,

    -- * Destructuring the Response
    batchExecuteStatementResponse,
    BatchExecuteStatementResponse,

    -- * Response Lenses
    besrsResponses,
    besrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchExecuteStatement' smart constructor.
newtype BatchExecuteStatement = BatchExecuteStatement'
  { _besStatements ::
      List1 BatchStatementRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchExecuteStatement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'besStatements' - The list of PartiQL statements representing the batch to run.
batchExecuteStatement ::
  -- | 'besStatements'
  NonEmpty BatchStatementRequest ->
  BatchExecuteStatement
batchExecuteStatement pStatements_ =
  BatchExecuteStatement' {_besStatements = _List1 # pStatements_}

-- | The list of PartiQL statements representing the batch to run.
besStatements :: Lens' BatchExecuteStatement (NonEmpty BatchStatementRequest)
besStatements = lens _besStatements (\s a -> s {_besStatements = a}) . _List1

instance AWSRequest BatchExecuteStatement where
  type Rs BatchExecuteStatement = BatchExecuteStatementResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            <$> (x .?> "Responses" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable BatchExecuteStatement

instance NFData BatchExecuteStatement

instance ToHeaders BatchExecuteStatement where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.BatchExecuteStatement" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    object (catMaybes [Just ("Statements" .= _besStatements)])

instance ToPath BatchExecuteStatement where
  toPath = const "/"

instance ToQuery BatchExecuteStatement where
  toQuery = const mempty

-- | /See:/ 'batchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { _besrsResponses ::
      !( Maybe
           [BatchStatementResponse]
       ),
    _besrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchExecuteStatementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'besrsResponses' - The response to each PartiQL statement in the batch.
--
-- * 'besrsResponseStatus' - -- | The response status code.
batchExecuteStatementResponse ::
  -- | 'besrsResponseStatus'
  Int ->
  BatchExecuteStatementResponse
batchExecuteStatementResponse pResponseStatus_ =
  BatchExecuteStatementResponse'
    { _besrsResponses = Nothing,
      _besrsResponseStatus = pResponseStatus_
    }

-- | The response to each PartiQL statement in the batch.
besrsResponses :: Lens' BatchExecuteStatementResponse [BatchStatementResponse]
besrsResponses = lens _besrsResponses (\s a -> s {_besrsResponses = a}) . _Default . _Coerce

-- | -- | The response status code.
besrsResponseStatus :: Lens' BatchExecuteStatementResponse Int
besrsResponseStatus = lens _besrsResponseStatus (\s a -> s {_besrsResponseStatus = a})

instance NFData BatchExecuteStatementResponse

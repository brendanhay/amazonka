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
-- Module      : Network.AWS.DynamoDB.ExecuteTransaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform transactional reads or writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.ExecuteTransaction
  ( -- * Creating a Request
    executeTransaction,
    ExecuteTransaction,

    -- * Request Lenses
    etClientRequestToken,
    etTransactStatements,

    -- * Destructuring the Response
    executeTransactionResponse,
    ExecuteTransactionResponse,

    -- * Response Lenses
    etrsResponses,
    etrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'executeTransaction' smart constructor.
data ExecuteTransaction = ExecuteTransaction'
  { _etClientRequestToken ::
      !(Maybe Text),
    _etTransactStatements ::
      !(List1 ParameterizedStatement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteTransaction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etClientRequestToken' - Set this value to get remaining results, if @NextToken@ was returned in the statement response.
--
-- * 'etTransactStatements' - The list of PartiQL statements representing the transaction to run.
executeTransaction ::
  -- | 'etTransactStatements'
  NonEmpty ParameterizedStatement ->
  ExecuteTransaction
executeTransaction pTransactStatements_ =
  ExecuteTransaction'
    { _etClientRequestToken = Nothing,
      _etTransactStatements = _List1 # pTransactStatements_
    }

-- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
etClientRequestToken :: Lens' ExecuteTransaction (Maybe Text)
etClientRequestToken = lens _etClientRequestToken (\s a -> s {_etClientRequestToken = a})

-- | The list of PartiQL statements representing the transaction to run.
etTransactStatements :: Lens' ExecuteTransaction (NonEmpty ParameterizedStatement)
etTransactStatements = lens _etTransactStatements (\s a -> s {_etTransactStatements = a}) . _List1

instance AWSRequest ExecuteTransaction where
  type Rs ExecuteTransaction = ExecuteTransactionResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          ExecuteTransactionResponse'
            <$> (x .?> "Responses") <*> (pure (fromEnum s))
      )

instance Hashable ExecuteTransaction

instance NFData ExecuteTransaction

instance ToHeaders ExecuteTransaction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.ExecuteTransaction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON ExecuteTransaction where
  toJSON ExecuteTransaction' {..} =
    object
      ( catMaybes
          [ ("ClientRequestToken" .=) <$> _etClientRequestToken,
            Just ("TransactStatements" .= _etTransactStatements)
          ]
      )

instance ToPath ExecuteTransaction where
  toPath = const "/"

instance ToQuery ExecuteTransaction where
  toQuery = const mempty

-- | /See:/ 'executeTransactionResponse' smart constructor.
data ExecuteTransactionResponse = ExecuteTransactionResponse'
  { _etrsResponses ::
      !(Maybe (List1 ItemResponse)),
    _etrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteTransactionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etrsResponses' - The response to a PartiQL transaction.
--
-- * 'etrsResponseStatus' - -- | The response status code.
executeTransactionResponse ::
  -- | 'etrsResponseStatus'
  Int ->
  ExecuteTransactionResponse
executeTransactionResponse pResponseStatus_ =
  ExecuteTransactionResponse'
    { _etrsResponses = Nothing,
      _etrsResponseStatus = pResponseStatus_
    }

-- | The response to a PartiQL transaction.
etrsResponses :: Lens' ExecuteTransactionResponse (Maybe (NonEmpty ItemResponse))
etrsResponses = lens _etrsResponses (\s a -> s {_etrsResponses = a}) . mapping _List1

-- | -- | The response status code.
etrsResponseStatus :: Lens' ExecuteTransactionResponse Int
etrsResponseStatus = lens _etrsResponseStatus (\s a -> s {_etrsResponseStatus = a})

instance NFData ExecuteTransactionResponse

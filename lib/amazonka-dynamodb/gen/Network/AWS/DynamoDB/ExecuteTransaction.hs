{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    ExecuteTransaction (..),
    mkExecuteTransaction,

    -- ** Request lenses
    etTransactStatements,
    etClientRequestToken,

    -- * Destructuring the response
    ExecuteTransactionResponse (..),
    mkExecuteTransactionResponse,

    -- ** Response lenses
    etrsResponses,
    etrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExecuteTransaction' smart constructor.
data ExecuteTransaction = ExecuteTransaction'
  { -- | The list of PartiQL statements representing the transaction to run.
    transactStatements :: Lude.NonEmpty ParameterizedStatement,
    -- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteTransaction' with the minimum fields required to make a request.
--
-- * 'transactStatements' - The list of PartiQL statements representing the transaction to run.
-- * 'clientRequestToken' - Set this value to get remaining results, if @NextToken@ was returned in the statement response.
mkExecuteTransaction ::
  -- | 'transactStatements'
  Lude.NonEmpty ParameterizedStatement ->
  ExecuteTransaction
mkExecuteTransaction pTransactStatements_ =
  ExecuteTransaction'
    { transactStatements = pTransactStatements_,
      clientRequestToken = Lude.Nothing
    }

-- | The list of PartiQL statements representing the transaction to run.
--
-- /Note:/ Consider using 'transactStatements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTransactStatements :: Lens.Lens' ExecuteTransaction (Lude.NonEmpty ParameterizedStatement)
etTransactStatements = Lens.lens (transactStatements :: ExecuteTransaction -> Lude.NonEmpty ParameterizedStatement) (\s a -> s {transactStatements = a} :: ExecuteTransaction)
{-# DEPRECATED etTransactStatements "Use generic-lens or generic-optics with 'transactStatements' instead." #-}

-- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etClientRequestToken :: Lens.Lens' ExecuteTransaction (Lude.Maybe Lude.Text)
etClientRequestToken = Lens.lens (clientRequestToken :: ExecuteTransaction -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: ExecuteTransaction)
{-# DEPRECATED etClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest ExecuteTransaction where
  type Rs ExecuteTransaction = ExecuteTransactionResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExecuteTransactionResponse'
            Lude.<$> (x Lude..?> "Responses") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExecuteTransaction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ExecuteTransaction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExecuteTransaction where
  toJSON ExecuteTransaction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransactStatements" Lude..= transactStatements),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath ExecuteTransaction where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecuteTransaction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExecuteTransactionResponse' smart constructor.
data ExecuteTransactionResponse = ExecuteTransactionResponse'
  { -- | The response to a PartiQL transaction.
    responses :: Lude.Maybe (Lude.NonEmpty ItemResponse),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteTransactionResponse' with the minimum fields required to make a request.
--
-- * 'responses' - The response to a PartiQL transaction.
-- * 'responseStatus' - The response status code.
mkExecuteTransactionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExecuteTransactionResponse
mkExecuteTransactionResponse pResponseStatus_ =
  ExecuteTransactionResponse'
    { responses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The response to a PartiQL transaction.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrsResponses :: Lens.Lens' ExecuteTransactionResponse (Lude.Maybe (Lude.NonEmpty ItemResponse))
etrsResponses = Lens.lens (responses :: ExecuteTransactionResponse -> Lude.Maybe (Lude.NonEmpty ItemResponse)) (\s a -> s {responses = a} :: ExecuteTransactionResponse)
{-# DEPRECATED etrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrsResponseStatus :: Lens.Lens' ExecuteTransactionResponse Lude.Int
etrsResponseStatus = Lens.lens (responseStatus :: ExecuteTransactionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExecuteTransactionResponse)
{-# DEPRECATED etrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

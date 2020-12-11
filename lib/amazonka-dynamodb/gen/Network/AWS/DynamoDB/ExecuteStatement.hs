{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    ExecuteStatement (..),
    mkExecuteStatement,

    -- ** Request lenses
    esConsistentRead,
    esNextToken,
    esParameters,
    esStatement,

    -- * Destructuring the response
    ExecuteStatementResponse (..),
    mkExecuteStatementResponse,

    -- ** Response lenses
    esrsItems,
    esrsNextToken,
    esrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { consistentRead ::
      Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe (Lude.NonEmpty AttributeValue),
    statement :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteStatement' with the minimum fields required to make a request.
--
-- * 'consistentRead' - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
-- * 'nextToken' - Set this value to get remaining results, if @NextToken@ was returned in the statement response.
-- * 'parameters' - The parameters for the PartiQL statement, if any.
-- * 'statement' - The PartiQL statement representing the operation to run.
mkExecuteStatement ::
  -- | 'statement'
  Lude.Text ->
  ExecuteStatement
mkExecuteStatement pStatement_ =
  ExecuteStatement'
    { consistentRead = Lude.Nothing,
      nextToken = Lude.Nothing,
      parameters = Lude.Nothing,
      statement = pStatement_
    }

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esConsistentRead :: Lens.Lens' ExecuteStatement (Lude.Maybe Lude.Bool)
esConsistentRead = Lens.lens (consistentRead :: ExecuteStatement -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: ExecuteStatement)
{-# DEPRECATED esConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | Set this value to get remaining results, if @NextToken@ was returned in the statement response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esNextToken :: Lens.Lens' ExecuteStatement (Lude.Maybe Lude.Text)
esNextToken = Lens.lens (nextToken :: ExecuteStatement -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ExecuteStatement)
{-# DEPRECATED esNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameters for the PartiQL statement, if any.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esParameters :: Lens.Lens' ExecuteStatement (Lude.Maybe (Lude.NonEmpty AttributeValue))
esParameters = Lens.lens (parameters :: ExecuteStatement -> Lude.Maybe (Lude.NonEmpty AttributeValue)) (\s a -> s {parameters = a} :: ExecuteStatement)
{-# DEPRECATED esParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The PartiQL statement representing the operation to run.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatement :: Lens.Lens' ExecuteStatement Lude.Text
esStatement = Lens.lens (statement :: ExecuteStatement -> Lude.Text) (\s a -> s {statement = a} :: ExecuteStatement)
{-# DEPRECATED esStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

instance Lude.AWSRequest ExecuteStatement where
  type Rs ExecuteStatement = ExecuteStatementResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            Lude.<$> (x Lude..?> "Items" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExecuteStatement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ExecuteStatement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConsistentRead" Lude..=) Lude.<$> consistentRead,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Statement" Lude..= statement)
          ]
      )

instance Lude.ToPath ExecuteStatement where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecuteStatement where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { items ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (AttributeValue)
        ],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteStatementResponse' with the minimum fields required to make a request.
--
-- * 'items' - If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
-- * 'nextToken' - If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
-- * 'responseStatus' - The response status code.
mkExecuteStatementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExecuteStatementResponse
mkExecuteStatementResponse pResponseStatus_ =
  ExecuteStatementResponse'
    { items = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a read operation was used, this property will contain the result of the reade operation; a map of attribute names and their values. For the write operations this value will be empty.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsItems :: Lens.Lens' ExecuteStatementResponse (Lude.Maybe [Lude.HashMap Lude.Text (AttributeValue)])
esrsItems = Lens.lens (items :: ExecuteStatementResponse -> Lude.Maybe [Lude.HashMap Lude.Text (AttributeValue)]) (\s a -> s {items = a} :: ExecuteStatementResponse)
{-# DEPRECATED esrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If the response of a read request exceeds the response payload limit DynamoDB will set this value in the response. If set, you can use that this value in the subsequent request to get the remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsNextToken :: Lens.Lens' ExecuteStatementResponse (Lude.Maybe Lude.Text)
esrsNextToken = Lens.lens (nextToken :: ExecuteStatementResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ExecuteStatementResponse)
{-# DEPRECATED esrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' ExecuteStatementResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: ExecuteStatementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExecuteStatementResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.BatchGetNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single named query or a list of up to 50 queries, which you provide as an array of query ID strings. Requires you to have access to the workgroup in which the queries were saved. Use 'ListNamedQueriesInput' to get the list of named query IDs in the specified workgroup. If information could not be retrieved for a submitted query ID, information about the query ID submitted is listed under 'UnprocessedNamedQueryId' . Named queries differ from executed queries. Use 'BatchGetQueryExecutionInput' to get details about each unique query execution, and 'ListQueryExecutionsInput' to get a list of query execution IDs.
module Network.AWS.Athena.BatchGetNamedQuery
  ( -- * Creating a request
    BatchGetNamedQuery (..),
    mkBatchGetNamedQuery,

    -- ** Request lenses
    bgnqNamedQueryIds,

    -- * Destructuring the response
    BatchGetNamedQueryResponse (..),
    mkBatchGetNamedQueryResponse,

    -- ** Response lenses
    bgnqrsNamedQueries,
    bgnqrsUnprocessedNamedQueryIds,
    bgnqrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetNamedQuery' smart constructor.
newtype BatchGetNamedQuery = BatchGetNamedQuery'
  { -- | An array of query IDs.
    namedQueryIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetNamedQuery' with the minimum fields required to make a request.
--
-- * 'namedQueryIds' - An array of query IDs.
mkBatchGetNamedQuery ::
  -- | 'namedQueryIds'
  Lude.NonEmpty Lude.Text ->
  BatchGetNamedQuery
mkBatchGetNamedQuery pNamedQueryIds_ =
  BatchGetNamedQuery' {namedQueryIds = pNamedQueryIds_}

-- | An array of query IDs.
--
-- /Note:/ Consider using 'namedQueryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqNamedQueryIds :: Lens.Lens' BatchGetNamedQuery (Lude.NonEmpty Lude.Text)
bgnqNamedQueryIds = Lens.lens (namedQueryIds :: BatchGetNamedQuery -> Lude.NonEmpty Lude.Text) (\s a -> s {namedQueryIds = a} :: BatchGetNamedQuery)
{-# DEPRECATED bgnqNamedQueryIds "Use generic-lens or generic-optics with 'namedQueryIds' instead." #-}

instance Lude.AWSRequest BatchGetNamedQuery where
  type Rs BatchGetNamedQuery = BatchGetNamedQueryResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetNamedQueryResponse'
            Lude.<$> (x Lude..?> "NamedQueries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnprocessedNamedQueryIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetNamedQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.BatchGetNamedQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetNamedQuery where
  toJSON BatchGetNamedQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NamedQueryIds" Lude..= namedQueryIds)]
      )

instance Lude.ToPath BatchGetNamedQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetNamedQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetNamedQueryResponse' smart constructor.
data BatchGetNamedQueryResponse = BatchGetNamedQueryResponse'
  { -- | Information about the named query IDs submitted.
    namedQueries :: Lude.Maybe [NamedQuery],
    -- | Information about provided query IDs.
    unprocessedNamedQueryIds :: Lude.Maybe [UnprocessedNamedQueryId],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetNamedQueryResponse' with the minimum fields required to make a request.
--
-- * 'namedQueries' - Information about the named query IDs submitted.
-- * 'unprocessedNamedQueryIds' - Information about provided query IDs.
-- * 'responseStatus' - The response status code.
mkBatchGetNamedQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetNamedQueryResponse
mkBatchGetNamedQueryResponse pResponseStatus_ =
  BatchGetNamedQueryResponse'
    { namedQueries = Lude.Nothing,
      unprocessedNamedQueryIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the named query IDs submitted.
--
-- /Note:/ Consider using 'namedQueries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrsNamedQueries :: Lens.Lens' BatchGetNamedQueryResponse (Lude.Maybe [NamedQuery])
bgnqrsNamedQueries = Lens.lens (namedQueries :: BatchGetNamedQueryResponse -> Lude.Maybe [NamedQuery]) (\s a -> s {namedQueries = a} :: BatchGetNamedQueryResponse)
{-# DEPRECATED bgnqrsNamedQueries "Use generic-lens or generic-optics with 'namedQueries' instead." #-}

-- | Information about provided query IDs.
--
-- /Note:/ Consider using 'unprocessedNamedQueryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrsUnprocessedNamedQueryIds :: Lens.Lens' BatchGetNamedQueryResponse (Lude.Maybe [UnprocessedNamedQueryId])
bgnqrsUnprocessedNamedQueryIds = Lens.lens (unprocessedNamedQueryIds :: BatchGetNamedQueryResponse -> Lude.Maybe [UnprocessedNamedQueryId]) (\s a -> s {unprocessedNamedQueryIds = a} :: BatchGetNamedQueryResponse)
{-# DEPRECATED bgnqrsUnprocessedNamedQueryIds "Use generic-lens or generic-optics with 'unprocessedNamedQueryIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrsResponseStatus :: Lens.Lens' BatchGetNamedQueryResponse Lude.Int
bgnqrsResponseStatus = Lens.lens (responseStatus :: BatchGetNamedQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetNamedQueryResponse)
{-# DEPRECATED bgnqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

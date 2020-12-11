{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the named query if you have access to the workgroup in which the query was saved.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.DeleteNamedQuery
  ( -- * Creating a request
    DeleteNamedQuery (..),
    mkDeleteNamedQuery,

    -- ** Request lenses
    dnqNamedQueryId,

    -- * Destructuring the response
    DeleteNamedQueryResponse (..),
    mkDeleteNamedQueryResponse,

    -- ** Response lenses
    dnqrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNamedQuery' smart constructor.
newtype DeleteNamedQuery = DeleteNamedQuery'
  { namedQueryId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNamedQuery' with the minimum fields required to make a request.
--
-- * 'namedQueryId' - The unique ID of the query to delete.
mkDeleteNamedQuery ::
  -- | 'namedQueryId'
  Lude.Text ->
  DeleteNamedQuery
mkDeleteNamedQuery pNamedQueryId_ =
  DeleteNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query to delete.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqNamedQueryId :: Lens.Lens' DeleteNamedQuery Lude.Text
dnqNamedQueryId = Lens.lens (namedQueryId :: DeleteNamedQuery -> Lude.Text) (\s a -> s {namedQueryId = a} :: DeleteNamedQuery)
{-# DEPRECATED dnqNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

instance Lude.AWSRequest DeleteNamedQuery where
  type Rs DeleteNamedQuery = DeleteNamedQueryResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteNamedQueryResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNamedQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.DeleteNamedQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNamedQuery where
  toJSON DeleteNamedQuery' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("NamedQueryId" Lude..= namedQueryId)])

instance Lude.ToPath DeleteNamedQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNamedQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNamedQueryResponse' smart constructor.
newtype DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNamedQueryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteNamedQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNamedQueryResponse
mkDeleteNamedQueryResponse pResponseStatus_ =
  DeleteNamedQueryResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqrsResponseStatus :: Lens.Lens' DeleteNamedQueryResponse Lude.Int
dnqrsResponseStatus = Lens.lens (responseStatus :: DeleteNamedQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNamedQueryResponse)
{-# DEPRECATED dnqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

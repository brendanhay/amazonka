{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query. Requires that you have access to the workgroup in which the query was saved.
module Network.AWS.Athena.GetNamedQuery
  ( -- * Creating a request
    GetNamedQuery (..),
    mkGetNamedQuery,

    -- ** Request lenses
    gnqNamedQueryId,

    -- * Destructuring the response
    GetNamedQueryResponse (..),
    mkGetNamedQueryResponse,

    -- ** Response lenses
    gnqrsNamedQuery,
    gnqrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetNamedQuery' smart constructor.
newtype GetNamedQuery = GetNamedQuery'
  { -- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
    namedQueryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNamedQuery' with the minimum fields required to make a request.
--
-- * 'namedQueryId' - The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
mkGetNamedQuery ::
  -- | 'namedQueryId'
  Lude.Text ->
  GetNamedQuery
mkGetNamedQuery pNamedQueryId_ =
  GetNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqNamedQueryId :: Lens.Lens' GetNamedQuery Lude.Text
gnqNamedQueryId = Lens.lens (namedQueryId :: GetNamedQuery -> Lude.Text) (\s a -> s {namedQueryId = a} :: GetNamedQuery)
{-# DEPRECATED gnqNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

instance Lude.AWSRequest GetNamedQuery where
  type Rs GetNamedQuery = GetNamedQueryResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetNamedQueryResponse'
            Lude.<$> (x Lude..?> "NamedQuery") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetNamedQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetNamedQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetNamedQuery where
  toJSON GetNamedQuery' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("NamedQueryId" Lude..= namedQueryId)])

instance Lude.ToPath GetNamedQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery GetNamedQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { -- | Information about the query.
    namedQuery :: Lude.Maybe NamedQuery,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNamedQueryResponse' with the minimum fields required to make a request.
--
-- * 'namedQuery' - Information about the query.
-- * 'responseStatus' - The response status code.
mkGetNamedQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetNamedQueryResponse
mkGetNamedQueryResponse pResponseStatus_ =
  GetNamedQueryResponse'
    { namedQuery = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the query.
--
-- /Note:/ Consider using 'namedQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrsNamedQuery :: Lens.Lens' GetNamedQueryResponse (Lude.Maybe NamedQuery)
gnqrsNamedQuery = Lens.lens (namedQuery :: GetNamedQueryResponse -> Lude.Maybe NamedQuery) (\s a -> s {namedQuery = a} :: GetNamedQueryResponse)
{-# DEPRECATED gnqrsNamedQuery "Use generic-lens or generic-optics with 'namedQuery' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnqrsResponseStatus :: Lens.Lens' GetNamedQueryResponse Lude.Int
gnqrsResponseStatus = Lens.lens (responseStatus :: GetNamedQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetNamedQueryResponse)
{-# DEPRECATED gnqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available log streams for a specific database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
  ( -- * Creating a request
    GetRelationalDatabaseLogStreams (..),
    mkGetRelationalDatabaseLogStreams,

    -- ** Request lenses
    grdlsRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseLogStreamsResponse (..),
    mkGetRelationalDatabaseLogStreamsResponse,

    -- ** Response lenses
    grdlsrsLogStreams,
    grdlsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseLogStreams' smart constructor.
newtype GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { relationalDatabaseName ::
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

-- | Creates a value of 'GetRelationalDatabaseLogStreams' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseName' - The name of your database for which to get log streams.
mkGetRelationalDatabaseLogStreams ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  GetRelationalDatabaseLogStreams
mkGetRelationalDatabaseLogStreams pRelationalDatabaseName_ =
  GetRelationalDatabaseLogStreams'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database for which to get log streams.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogStreams Lude.Text
grdlsRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabaseLogStreams -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseLogStreams)
{-# DEPRECATED grdlsRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest GetRelationalDatabaseLogStreams where
  type
    Rs GetRelationalDatabaseLogStreams =
      GetRelationalDatabaseLogStreamsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogStreamsResponse'
            Lude.<$> (x Lude..?> "logStreams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseLogStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseLogStreams" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseLogStreams where
  toJSON GetRelationalDatabaseLogStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath GetRelationalDatabaseLogStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseLogStreams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { logStreams ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseLogStreamsResponse' with the minimum fields required to make a request.
--
-- * 'logStreams' - An object describing the result of your get relational database log streams request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseLogStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseLogStreamsResponse
mkGetRelationalDatabaseLogStreamsResponse pResponseStatus_ =
  GetRelationalDatabaseLogStreamsResponse'
    { logStreams =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object describing the result of your get relational database log streams request.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsrsLogStreams :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse (Lude.Maybe [Lude.Text])
grdlsrsLogStreams = Lens.lens (logStreams :: GetRelationalDatabaseLogStreamsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {logStreams = a} :: GetRelationalDatabaseLogStreamsResponse)
{-# DEPRECATED grdlsrsLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsrsResponseStatus :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse Lude.Int
grdlsrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseLogStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseLogStreamsResponse)
{-# DEPRECATED grdlsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

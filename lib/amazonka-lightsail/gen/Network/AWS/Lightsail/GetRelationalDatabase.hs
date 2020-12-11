{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabase
  ( -- * Creating a request
    GetRelationalDatabase (..),
    mkGetRelationalDatabase,

    -- ** Request lenses
    grdRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseResponse (..),
    mkGetRelationalDatabaseResponse,

    -- ** Response lenses
    grdrrsRelationalDatabase,
    grdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabase' smart constructor.
newtype GetRelationalDatabase = GetRelationalDatabase'
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

-- | Creates a value of 'GetRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseName' - The name of the database that you are looking up.
mkGetRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  GetRelationalDatabase
mkGetRelationalDatabase pRelationalDatabaseName_ =
  GetRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of the database that you are looking up.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdRelationalDatabaseName :: Lens.Lens' GetRelationalDatabase Lude.Text
grdRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabase)
{-# DEPRECATED grdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest GetRelationalDatabase where
  type Rs GetRelationalDatabase = GetRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "relationalDatabase")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabase where
  toJSON GetRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath GetRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseResponse' smart constructor.
data GetRelationalDatabaseResponse = GetRelationalDatabaseResponse'
  { relationalDatabase ::
      Lude.Maybe RelationalDatabase,
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

-- | Creates a value of 'GetRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'relationalDatabase' - An object describing the specified database.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseResponse
mkGetRelationalDatabaseResponse pResponseStatus_ =
  GetRelationalDatabaseResponse'
    { relationalDatabase = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object describing the specified database.
--
-- /Note:/ Consider using 'relationalDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsRelationalDatabase :: Lens.Lens' GetRelationalDatabaseResponse (Lude.Maybe RelationalDatabase)
grdrrsRelationalDatabase = Lens.lens (relationalDatabase :: GetRelationalDatabaseResponse -> Lude.Maybe RelationalDatabase) (\s a -> s {relationalDatabase = a} :: GetRelationalDatabaseResponse)
{-# DEPRECATED grdrrsRelationalDatabase "Use generic-lens or generic-optics with 'relationalDatabase' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsResponseStatus :: Lens.Lens' GetRelationalDatabaseResponse Lude.Int
grdrrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseResponse)
{-# DEPRECATED grdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

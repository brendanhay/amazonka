{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specfied database and data catalog.
module Network.AWS.Athena.GetDatabase
  ( -- * Creating a request
    GetDatabase (..),
    mkGetDatabase,

    -- ** Request lenses
    gdCatalogName,
    gdDatabaseName,

    -- * Destructuring the response
    GetDatabaseResponse (..),
    mkGetDatabaseResponse,

    -- ** Response lenses
    gdrsDatabase,
    gdrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The name of the data catalog that contains the database to return.
    catalogName :: Lude.Text,
    -- | The name of the database to return.
    databaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatabase' with the minimum fields required to make a request.
--
-- * 'catalogName' - The name of the data catalog that contains the database to return.
-- * 'databaseName' - The name of the database to return.
mkGetDatabase ::
  -- | 'catalogName'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  GetDatabase
mkGetDatabase pCatalogName_ pDatabaseName_ =
  GetDatabase'
    { catalogName = pCatalogName_,
      databaseName = pDatabaseName_
    }

-- | The name of the data catalog that contains the database to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCatalogName :: Lens.Lens' GetDatabase Lude.Text
gdCatalogName = Lens.lens (catalogName :: GetDatabase -> Lude.Text) (\s a -> s {catalogName = a} :: GetDatabase)
{-# DEPRECATED gdCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The name of the database to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDatabaseName :: Lens.Lens' GetDatabase Lude.Text
gdDatabaseName = Lens.lens (databaseName :: GetDatabase -> Lude.Text) (\s a -> s {databaseName = a} :: GetDatabase)
{-# DEPRECATED gdDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.AWSRequest GetDatabase where
  type Rs GetDatabase = GetDatabaseResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Lude.<$> (x Lude..?> "Database") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CatalogName" Lude..= catalogName),
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )

instance Lude.ToPath GetDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { -- | The database returned.
    database :: Lude.Maybe Database,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'database' - The database returned.
-- * 'responseStatus' - The response status code.
mkGetDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDatabaseResponse
mkGetDatabaseResponse pResponseStatus_ =
  GetDatabaseResponse'
    { database = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The database returned.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDatabase :: Lens.Lens' GetDatabaseResponse (Lude.Maybe Database)
gdrsDatabase = Lens.lens (database :: GetDatabaseResponse -> Lude.Maybe Database) (\s a -> s {database = a} :: GetDatabaseResponse)
{-# DEPRECATED gdrsDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDatabaseResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDatabaseResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

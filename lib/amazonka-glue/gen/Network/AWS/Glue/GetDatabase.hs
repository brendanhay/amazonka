{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a specified database.
module Network.AWS.Glue.GetDatabase
  ( -- * Creating a request
    GetDatabase (..),
    mkGetDatabase,

    -- ** Request lenses
    gddCatalogId,
    gddName,

    -- * Destructuring the response
    GetDatabaseResponse (..),
    mkGetDatabaseResponse,

    -- ** Response lenses
    gdrsDatabase,
    gdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { catalogId :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatabase' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
-- * 'name' - The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
mkGetDatabase ::
  -- | 'name'
  Lude.Text ->
  GetDatabase
mkGetDatabase pName_ =
  GetDatabase' {catalogId = Lude.Nothing, name = pName_}

-- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddCatalogId :: Lens.Lens' GetDatabase (Lude.Maybe Lude.Text)
gddCatalogId = Lens.lens (catalogId :: GetDatabase -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetDatabase)
{-# DEPRECATED gddCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddName :: Lens.Lens' GetDatabase Lude.Text
gddName = Lens.lens (name :: GetDatabase -> Lude.Text) (\s a -> s {name = a} :: GetDatabase)
{-# DEPRECATED gddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetDatabase where
  type Rs GetDatabase = GetDatabaseResponse
  request = Req.postJSON glueService
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
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { database ::
      Lude.Maybe Database,
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

-- | Creates a value of 'GetDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'database' - The definition of the specified database in the Data Catalog.
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

-- | The definition of the specified database in the Data Catalog.
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

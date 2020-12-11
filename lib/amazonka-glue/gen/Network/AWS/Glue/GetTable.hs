{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @Table@ definition in a Data Catalog for a specified table.
module Network.AWS.Glue.GetTable
  ( -- * Creating a request
    GetTable (..),
    mkGetTable,

    -- ** Request lenses
    gttCatalogId,
    gttDatabaseName,
    gttName,

    -- * Destructuring the response
    GetTableResponse (..),
    mkGetTableResponse,

    -- ** Response lenses
    getersTable,
    getersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTable' smart constructor.
data GetTable = GetTable'
  { catalogId :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
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

-- | Creates a value of 'GetTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'name' - The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
mkGetTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GetTable
mkGetTable pDatabaseName_ pName_ =
  GetTable'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttCatalogId :: Lens.Lens' GetTable (Lude.Maybe Lude.Text)
gttCatalogId = Lens.lens (catalogId :: GetTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTable)
{-# DEPRECATED gttCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttDatabaseName :: Lens.Lens' GetTable Lude.Text
gttDatabaseName = Lens.lens (databaseName :: GetTable -> Lude.Text) (\s a -> s {databaseName = a} :: GetTable)
{-# DEPRECATED gttDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gttName :: Lens.Lens' GetTable Lude.Text
gttName = Lens.lens (name :: GetTable -> Lude.Text) (\s a -> s {name = a} :: GetTable)
{-# DEPRECATED gttName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetTable where
  type Rs GetTable = GetTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTableResponse'
            Lude.<$> (x Lude..?> "Table") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTable where
  toJSON GetTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetTable where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { table ::
      Lude.Maybe Table,
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

-- | Creates a value of 'GetTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'table' - The @Table@ object that defines the specified table.
mkGetTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTableResponse
mkGetTableResponse pResponseStatus_ =
  GetTableResponse'
    { table = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Table@ object that defines the specified table.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getersTable :: Lens.Lens' GetTableResponse (Lude.Maybe Table)
getersTable = Lens.lens (table :: GetTableResponse -> Lude.Maybe Table) (\s a -> s {table = a} :: GetTableResponse)
{-# DEPRECATED getersTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getersResponseStatus :: Lens.Lens' GetTableResponse Lude.Int
getersResponseStatus = Lens.lens (responseStatus :: GetTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTableResponse)
{-# DEPRECATED getersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

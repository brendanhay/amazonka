{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gtfCatalogId,
    gtfName,
    gtfDatabaseName,

    -- * Destructuring the response
    GetTableResponse (..),
    mkGetTableResponse,

    -- ** Response lenses
    gthrsTable,
    gthrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTable' smart constructor.
data GetTable = GetTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
    name :: Lude.Text,
    -- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
-- * 'name' - The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
-- * 'databaseName' - The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
mkGetTable ::
  -- | 'name'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  GetTable
mkGetTable pName_ pDatabaseName_ =
  GetTable'
    { catalogId = Lude.Nothing,
      name = pName_,
      databaseName = pDatabaseName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfCatalogId :: Lens.Lens' GetTable (Lude.Maybe Lude.Text)
gtfCatalogId = Lens.lens (catalogId :: GetTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTable)
{-# DEPRECATED gtfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfName :: Lens.Lens' GetTable Lude.Text
gtfName = Lens.lens (name :: GetTable -> Lude.Text) (\s a -> s {name = a} :: GetTable)
{-# DEPRECATED gtfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfDatabaseName :: Lens.Lens' GetTable Lude.Text
gtfDatabaseName = Lens.lens (databaseName :: GetTable -> Lude.Text) (\s a -> s {databaseName = a} :: GetTable)
{-# DEPRECATED gtfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

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
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )

instance Lude.ToPath GetTable where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { -- | The @Table@ object that defines the specified table.
    table :: Lude.Maybe Table,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTableResponse' with the minimum fields required to make a request.
--
-- * 'table' - The @Table@ object that defines the specified table.
-- * 'responseStatus' - The response status code.
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
gthrsTable :: Lens.Lens' GetTableResponse (Lude.Maybe Table)
gthrsTable = Lens.lens (table :: GetTableResponse -> Lude.Maybe Table) (\s a -> s {table = a} :: GetTableResponse)
{-# DEPRECATED gthrsTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gthrsResponseStatus :: Lens.Lens' GetTableResponse Lude.Int
gthrsResponseStatus = Lens.lens (responseStatus :: GetTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTableResponse)
{-# DEPRECATED gthrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

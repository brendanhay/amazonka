{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetTableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table metadata for the specified catalog, database, and table.
module Network.AWS.Athena.GetTableMetadata
  ( -- * Creating a request
    GetTableMetadata (..),
    mkGetTableMetadata,

    -- ** Request lenses
    gtmCatalogName,
    gtmDatabaseName,
    gtmTableName,

    -- * Destructuring the response
    GetTableMetadataResponse (..),
    mkGetTableMetadataResponse,

    -- ** Response lenses
    gtmrsTableMetadata,
    gtmrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { -- | The name of the data catalog that contains the database and table metadata to return.
    catalogName :: Lude.Text,
    -- | The name of the database that contains the table metadata to return.
    databaseName :: Lude.Text,
    -- | The name of the table for which metadata is returned.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTableMetadata' with the minimum fields required to make a request.
--
-- * 'catalogName' - The name of the data catalog that contains the database and table metadata to return.
-- * 'databaseName' - The name of the database that contains the table metadata to return.
-- * 'tableName' - The name of the table for which metadata is returned.
mkGetTableMetadata ::
  -- | 'catalogName'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetTableMetadata
mkGetTableMetadata pCatalogName_ pDatabaseName_ pTableName_ =
  GetTableMetadata'
    { catalogName = pCatalogName_,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The name of the data catalog that contains the database and table metadata to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmCatalogName :: Lens.Lens' GetTableMetadata Lude.Text
gtmCatalogName = Lens.lens (catalogName :: GetTableMetadata -> Lude.Text) (\s a -> s {catalogName = a} :: GetTableMetadata)
{-# DEPRECATED gtmCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The name of the database that contains the table metadata to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmDatabaseName :: Lens.Lens' GetTableMetadata Lude.Text
gtmDatabaseName = Lens.lens (databaseName :: GetTableMetadata -> Lude.Text) (\s a -> s {databaseName = a} :: GetTableMetadata)
{-# DEPRECATED gtmDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table for which metadata is returned.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmTableName :: Lens.Lens' GetTableMetadata Lude.Text
gtmTableName = Lens.lens (tableName :: GetTableMetadata -> Lude.Text) (\s a -> s {tableName = a} :: GetTableMetadata)
{-# DEPRECATED gtmTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest GetTableMetadata where
  type Rs GetTableMetadata = GetTableMetadataResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTableMetadataResponse'
            Lude.<$> (x Lude..?> "TableMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTableMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetTableMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTableMetadata where
  toJSON GetTableMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CatalogName" Lude..= catalogName),
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetTableMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTableMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { -- | An object that contains table metadata.
    tableMetadata :: Lude.Maybe TableMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTableMetadataResponse' with the minimum fields required to make a request.
--
-- * 'tableMetadata' - An object that contains table metadata.
-- * 'responseStatus' - The response status code.
mkGetTableMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTableMetadataResponse
mkGetTableMetadataResponse pResponseStatus_ =
  GetTableMetadataResponse'
    { tableMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains table metadata.
--
-- /Note:/ Consider using 'tableMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrsTableMetadata :: Lens.Lens' GetTableMetadataResponse (Lude.Maybe TableMetadata)
gtmrsTableMetadata = Lens.lens (tableMetadata :: GetTableMetadataResponse -> Lude.Maybe TableMetadata) (\s a -> s {tableMetadata = a} :: GetTableMetadataResponse)
{-# DEPRECATED gtmrsTableMetadata "Use generic-lens or generic-optics with 'tableMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrsResponseStatus :: Lens.Lens' GetTableMetadataResponse Lude.Int
gtmrsResponseStatus = Lens.lens (responseStatus :: GetTableMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTableMetadataResponse)
{-# DEPRECATED gtmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

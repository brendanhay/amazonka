{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified version of a table.
module Network.AWS.Glue.GetTableVersion
  ( -- * Creating a request
    GetTableVersion (..),
    mkGetTableVersion,

    -- ** Request lenses
    gtvVersionId,
    gtvCatalogId,
    gtvDatabaseName,
    gtvTableName,

    -- * Destructuring the response
    GetTableVersionResponse (..),
    mkGetTableVersionResponse,

    -- ** Response lenses
    gtvrsTableVersion,
    gtvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTableVersion' smart constructor.
data GetTableVersion = GetTableVersion'
  { versionId ::
      Lude.Maybe Lude.Text,
    catalogId :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTableVersion' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'tableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
-- * 'versionId' - The ID value of the table version to be retrieved. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
mkGetTableVersion ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetTableVersion
mkGetTableVersion pDatabaseName_ pTableName_ =
  GetTableVersion'
    { versionId = Lude.Nothing,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID value of the table version to be retrieved. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvVersionId :: Lens.Lens' GetTableVersion (Lude.Maybe Lude.Text)
gtvVersionId = Lens.lens (versionId :: GetTableVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: GetTableVersion)
{-# DEPRECATED gtvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvCatalogId :: Lens.Lens' GetTableVersion (Lude.Maybe Lude.Text)
gtvCatalogId = Lens.lens (catalogId :: GetTableVersion -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetTableVersion)
{-# DEPRECATED gtvCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvDatabaseName :: Lens.Lens' GetTableVersion Lude.Text
gtvDatabaseName = Lens.lens (databaseName :: GetTableVersion -> Lude.Text) (\s a -> s {databaseName = a} :: GetTableVersion)
{-# DEPRECATED gtvDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvTableName :: Lens.Lens' GetTableVersion Lude.Text
gtvTableName = Lens.lens (tableName :: GetTableVersion -> Lude.Text) (\s a -> s {tableName = a} :: GetTableVersion)
{-# DEPRECATED gtvTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest GetTableVersion where
  type Rs GetTableVersion = GetTableVersionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTableVersionResponse'
            Lude.<$> (x Lude..?> "TableVersion") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTableVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetTableVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTableVersion where
  toJSON GetTableVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionId" Lude..=) Lude.<$> versionId,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetTableVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTableVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTableVersionResponse' smart constructor.
data GetTableVersionResponse = GetTableVersionResponse'
  { tableVersion ::
      Lude.Maybe TableVersion,
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

-- | Creates a value of 'GetTableVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tableVersion' - The requested table version.
mkGetTableVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTableVersionResponse
mkGetTableVersionResponse pResponseStatus_ =
  GetTableVersionResponse'
    { tableVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested table version.
--
-- /Note:/ Consider using 'tableVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrsTableVersion :: Lens.Lens' GetTableVersionResponse (Lude.Maybe TableVersion)
gtvrsTableVersion = Lens.lens (tableVersion :: GetTableVersionResponse -> Lude.Maybe TableVersion) (\s a -> s {tableVersion = a} :: GetTableVersionResponse)
{-# DEPRECATED gtvrsTableVersion "Use generic-lens or generic-optics with 'tableVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrsResponseStatus :: Lens.Lens' GetTableVersionResponse Lude.Int
gtvrsResponseStatus = Lens.lens (responseStatus :: GetTableVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTableVersionResponse)
{-# DEPRECATED gtvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

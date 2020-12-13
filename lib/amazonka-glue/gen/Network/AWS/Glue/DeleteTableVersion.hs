{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteTableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified version of a table.
module Network.AWS.Glue.DeleteTableVersion
  ( -- * Creating a request
    DeleteTableVersion (..),
    mkDeleteTableVersion,

    -- ** Request lenses
    dtvVersionId,
    dtvCatalogId,
    dtvDatabaseName,
    dtvTableName,

    -- * Destructuring the response
    DeleteTableVersionResponse (..),
    mkDeleteTableVersionResponse,

    -- ** Response lenses
    dtvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTableVersion' smart constructor.
data DeleteTableVersion = DeleteTableVersion'
  { -- | The ID of the table version to be deleted. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
    versionId :: Lude.Text,
    -- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely lowercase.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTableVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of the table version to be deleted. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'tableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
mkDeleteTableVersion ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  DeleteTableVersion
mkDeleteTableVersion pVersionId_ pDatabaseName_ pTableName_ =
  DeleteTableVersion'
    { versionId = pVersionId_,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the table version to be deleted. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtvVersionId :: Lens.Lens' DeleteTableVersion Lude.Text
dtvVersionId = Lens.lens (versionId :: DeleteTableVersion -> Lude.Text) (\s a -> s {versionId = a} :: DeleteTableVersion)
{-# DEPRECATED dtvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtvCatalogId :: Lens.Lens' DeleteTableVersion (Lude.Maybe Lude.Text)
dtvCatalogId = Lens.lens (catalogId :: DeleteTableVersion -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteTableVersion)
{-# DEPRECATED dtvCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtvDatabaseName :: Lens.Lens' DeleteTableVersion Lude.Text
dtvDatabaseName = Lens.lens (databaseName :: DeleteTableVersion -> Lude.Text) (\s a -> s {databaseName = a} :: DeleteTableVersion)
{-# DEPRECATED dtvDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtvTableName :: Lens.Lens' DeleteTableVersion Lude.Text
dtvTableName = Lens.lens (tableName :: DeleteTableVersion -> Lude.Text) (\s a -> s {tableName = a} :: DeleteTableVersion)
{-# DEPRECATED dtvTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DeleteTableVersion where
  type Rs DeleteTableVersion = DeleteTableVersionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTableVersionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTableVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteTableVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTableVersion where
  toJSON DeleteTableVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("VersionId" Lude..= versionId),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath DeleteTableVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTableVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTableVersionResponse' smart constructor.
newtype DeleteTableVersionResponse = DeleteTableVersionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTableVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTableVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTableVersionResponse
mkDeleteTableVersionResponse pResponseStatus_ =
  DeleteTableVersionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtvrsResponseStatus :: Lens.Lens' DeleteTableVersionResponse Lude.Int
dtvrsResponseStatus = Lens.lens (responseStatus :: DeleteTableVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTableVersionResponse)
{-# DEPRECATED dtvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

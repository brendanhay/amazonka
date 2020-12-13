{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteTableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified batch of versions of a table.
module Network.AWS.Glue.BatchDeleteTableVersion
  ( -- * Creating a request
    BatchDeleteTableVersion (..),
    mkBatchDeleteTableVersion,

    -- ** Request lenses
    bdtvVersionIds,
    bdtvCatalogId,
    bdtvDatabaseName,
    bdtvTableName,

    -- * Destructuring the response
    BatchDeleteTableVersionResponse (..),
    mkBatchDeleteTableVersionResponse,

    -- ** Response lenses
    bdtvrsErrors,
    bdtvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { -- | A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
    versionIds :: [Lude.Text],
    -- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely lowercase.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteTableVersion' with the minimum fields required to make a request.
--
-- * 'versionIds' - A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
-- * 'catalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'tableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
mkBatchDeleteTableVersion ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  BatchDeleteTableVersion
mkBatchDeleteTableVersion pDatabaseName_ pTableName_ =
  BatchDeleteTableVersion'
    { versionIds = Lude.mempty,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvVersionIds :: Lens.Lens' BatchDeleteTableVersion [Lude.Text]
bdtvVersionIds = Lens.lens (versionIds :: BatchDeleteTableVersion -> [Lude.Text]) (\s a -> s {versionIds = a} :: BatchDeleteTableVersion)
{-# DEPRECATED bdtvVersionIds "Use generic-lens or generic-optics with 'versionIds' instead." #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvCatalogId :: Lens.Lens' BatchDeleteTableVersion (Lude.Maybe Lude.Text)
bdtvCatalogId = Lens.lens (catalogId :: BatchDeleteTableVersion -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchDeleteTableVersion)
{-# DEPRECATED bdtvCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvDatabaseName :: Lens.Lens' BatchDeleteTableVersion Lude.Text
bdtvDatabaseName = Lens.lens (databaseName :: BatchDeleteTableVersion -> Lude.Text) (\s a -> s {databaseName = a} :: BatchDeleteTableVersion)
{-# DEPRECATED bdtvDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvTableName :: Lens.Lens' BatchDeleteTableVersion Lude.Text
bdtvTableName = Lens.lens (tableName :: BatchDeleteTableVersion -> Lude.Text) (\s a -> s {tableName = a} :: BatchDeleteTableVersion)
{-# DEPRECATED bdtvTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest BatchDeleteTableVersion where
  type Rs BatchDeleteTableVersion = BatchDeleteTableVersionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteTableVersionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteTableVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchDeleteTableVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteTableVersion where
  toJSON BatchDeleteTableVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("VersionIds" Lude..= versionIds),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath BatchDeleteTableVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteTableVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { -- | A list of errors encountered while trying to delete the specified table versions.
    errors :: Lude.Maybe [TableVersionError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteTableVersionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of errors encountered while trying to delete the specified table versions.
-- * 'responseStatus' - The response status code.
mkBatchDeleteTableVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteTableVersionResponse
mkBatchDeleteTableVersionResponse pResponseStatus_ =
  BatchDeleteTableVersionResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of errors encountered while trying to delete the specified table versions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrsErrors :: Lens.Lens' BatchDeleteTableVersionResponse (Lude.Maybe [TableVersionError])
bdtvrsErrors = Lens.lens (errors :: BatchDeleteTableVersionResponse -> Lude.Maybe [TableVersionError]) (\s a -> s {errors = a} :: BatchDeleteTableVersionResponse)
{-# DEPRECATED bdtvrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrsResponseStatus :: Lens.Lens' BatchDeleteTableVersionResponse Lude.Int
bdtvrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteTableVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteTableVersionResponse)
{-# DEPRECATED bdtvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

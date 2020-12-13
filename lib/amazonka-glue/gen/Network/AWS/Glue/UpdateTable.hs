{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
module Network.AWS.Glue.UpdateTable
  ( -- * Creating a request
    UpdateTable (..),
    mkUpdateTable,

    -- ** Request lenses
    utSkipArchive,
    utCatalogId,
    utDatabaseName,
    utTableInput,

    -- * Destructuring the response
    UpdateTableResponse (..),
    mkUpdateTableResponse,

    -- ** Response lenses
    utrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
    skipArchive :: Lude.Maybe Lude.Bool,
    -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Lude.Text,
    -- | An updated @TableInput@ object to define the metadata table in the catalog.
    tableInput :: TableInput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTable' with the minimum fields required to make a request.
--
-- * 'skipArchive' - By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
-- * 'catalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'tableInput' - An updated @TableInput@ object to define the metadata table in the catalog.
mkUpdateTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableInput'
  TableInput ->
  UpdateTable
mkUpdateTable pDatabaseName_ pTableInput_ =
  UpdateTable'
    { skipArchive = Lude.Nothing,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableInput = pTableInput_
    }

-- | By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
--
-- /Note:/ Consider using 'skipArchive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSkipArchive :: Lens.Lens' UpdateTable (Lude.Maybe Lude.Bool)
utSkipArchive = Lens.lens (skipArchive :: UpdateTable -> Lude.Maybe Lude.Bool) (\s a -> s {skipArchive = a} :: UpdateTable)
{-# DEPRECATED utSkipArchive "Use generic-lens or generic-optics with 'skipArchive' instead." #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCatalogId :: Lens.Lens' UpdateTable (Lude.Maybe Lude.Text)
utCatalogId = Lens.lens (catalogId :: UpdateTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateTable)
{-# DEPRECATED utCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDatabaseName :: Lens.Lens' UpdateTable Lude.Text
utDatabaseName = Lens.lens (databaseName :: UpdateTable -> Lude.Text) (\s a -> s {databaseName = a} :: UpdateTable)
{-# DEPRECATED utDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | An updated @TableInput@ object to define the metadata table in the catalog.
--
-- /Note:/ Consider using 'tableInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTableInput :: Lens.Lens' UpdateTable TableInput
utTableInput = Lens.lens (tableInput :: UpdateTable -> TableInput) (\s a -> s {tableInput = a} :: UpdateTable)
{-# DEPRECATED utTableInput "Use generic-lens or generic-optics with 'tableInput' instead." #-}

instance Lude.AWSRequest UpdateTable where
  type Rs UpdateTable = UpdateTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateTableResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.UpdateTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkipArchive" Lude..=) Lude.<$> skipArchive,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableInput" Lude..= tableInput)
          ]
      )

instance Lude.ToPath UpdateTable where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTableResponse' smart constructor.
newtype UpdateTableResponse = UpdateTableResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTableResponse
mkUpdateTableResponse pResponseStatus_ =
  UpdateTableResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTableResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTableResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

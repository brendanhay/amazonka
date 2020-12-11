{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a table definition from the Data Catalog.
module Network.AWS.Glue.DeleteTable
  ( -- * Creating a request
    DeleteTable (..),
    mkDeleteTable,

    -- ** Request lenses
    dtCatalogId,
    dtDatabaseName,
    dtName,

    -- * Destructuring the response
    DeleteTableResponse (..),
    mkDeleteTableResponse,

    -- ** Response lenses
    dtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
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

-- | Creates a value of 'DeleteTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
-- * 'name' - The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
mkDeleteTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeleteTable
mkDeleteTable pDatabaseName_ pName_ =
  DeleteTable'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCatalogId :: Lens.Lens' DeleteTable (Lude.Maybe Lude.Text)
dtCatalogId = Lens.lens (catalogId :: DeleteTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteTable)
{-# DEPRECATED dtCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDatabaseName :: Lens.Lens' DeleteTable Lude.Text
dtDatabaseName = Lens.lens (databaseName :: DeleteTable -> Lude.Text) (\s a -> s {databaseName = a} :: DeleteTable)
{-# DEPRECATED dtDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtName :: Lens.Lens' DeleteTable Lude.Text
dtName = Lens.lens (name :: DeleteTable -> Lude.Text) (\s a -> s {name = a} :: DeleteTable)
{-# DEPRECATED dtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteTable where
  type Rs DeleteTable = DeleteTableResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTableResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.DeleteTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DeleteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTable where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTableResponse' smart constructor.
newtype DeleteTableResponse = DeleteTableResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTableResponse
mkDeleteTableResponse pResponseStatus_ =
  DeleteTableResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTableResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTableResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified database from a Data Catalog.
module Network.AWS.Glue.DeleteDatabase
  ( -- * Creating a request
    DeleteDatabase (..),
    mkDeleteDatabase,

    -- ** Request lenses
    ddCatalogId,
    ddName,

    -- * Destructuring the response
    DeleteDatabaseResponse (..),
    mkDeleteDatabaseResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDatabase' smart constructor.
data DeleteDatabase = DeleteDatabase'
  { -- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the database to delete. For Hive compatibility, this must be all lowercase.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatabase' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
-- * 'name' - The name of the database to delete. For Hive compatibility, this must be all lowercase.
mkDeleteDatabase ::
  -- | 'name'
  Lude.Text ->
  DeleteDatabase
mkDeleteDatabase pName_ =
  DeleteDatabase' {catalogId = Lude.Nothing, name = pName_}

-- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCatalogId :: Lens.Lens' DeleteDatabase (Lude.Maybe Lude.Text)
ddCatalogId = Lens.lens (catalogId :: DeleteDatabase -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteDatabase)
{-# DEPRECATED ddCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the database to delete. For Hive compatibility, this must be all lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DeleteDatabase Lude.Text
ddName = Lens.lens (name :: DeleteDatabase -> Lude.Text) (\s a -> s {name = a} :: DeleteDatabase)
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteDatabase where
  type Rs DeleteDatabase = DeleteDatabaseResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDatabaseResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDatabase where
  toJSON DeleteDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DeleteDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDatabaseResponse' smart constructor.
newtype DeleteDatabaseResponse = DeleteDatabaseResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDatabaseResponse
mkDeleteDatabaseResponse pResponseStatus_ =
  DeleteDatabaseResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDatabaseResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDatabaseResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

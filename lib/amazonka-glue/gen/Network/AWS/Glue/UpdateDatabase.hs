{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing database definition in a Data Catalog.
module Network.AWS.Glue.UpdateDatabase
  ( -- * Creating a request
    UpdateDatabase (..),
    mkUpdateDatabase,

    -- ** Request lenses
    udDatabaseInput,
    udCatalogId,
    udName,

    -- * Destructuring the response
    UpdateDatabaseResponse (..),
    mkUpdateDatabaseResponse,

    -- ** Response lenses
    udrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { -- | A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
    databaseInput :: DatabaseInput,
    -- | The ID of the Data Catalog in which the metadata database resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDatabase' with the minimum fields required to make a request.
--
-- * 'databaseInput' - A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
-- * 'catalogId' - The ID of the Data Catalog in which the metadata database resides. If none is provided, the AWS account ID is used by default.
-- * 'name' - The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
mkUpdateDatabase ::
  -- | 'databaseInput'
  DatabaseInput ->
  -- | 'name'
  Lude.Text ->
  UpdateDatabase
mkUpdateDatabase pDatabaseInput_ pName_ =
  UpdateDatabase'
    { databaseInput = pDatabaseInput_,
      catalogId = Lude.Nothing,
      name = pName_
    }

-- | A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
--
-- /Note:/ Consider using 'databaseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDatabaseInput :: Lens.Lens' UpdateDatabase DatabaseInput
udDatabaseInput = Lens.lens (databaseInput :: UpdateDatabase -> DatabaseInput) (\s a -> s {databaseInput = a} :: UpdateDatabase)
{-# DEPRECATED udDatabaseInput "Use generic-lens or generic-optics with 'databaseInput' instead." #-}

-- | The ID of the Data Catalog in which the metadata database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCatalogId :: Lens.Lens' UpdateDatabase (Lude.Maybe Lude.Text)
udCatalogId = Lens.lens (catalogId :: UpdateDatabase -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateDatabase)
{-# DEPRECATED udCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDatabase Lude.Text
udName = Lens.lens (name :: UpdateDatabase -> Lude.Text) (\s a -> s {name = a} :: UpdateDatabase)
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateDatabase where
  type Rs UpdateDatabase = UpdateDatabaseResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDatabaseResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDatabase where
  toJSON UpdateDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DatabaseInput" Lude..= databaseInput),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDatabaseResponse' smart constructor.
newtype UpdateDatabaseResponse = UpdateDatabaseResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDatabaseResponse
mkUpdateDatabaseResponse pResponseStatus_ =
  UpdateDatabaseResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDatabaseResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDatabaseResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

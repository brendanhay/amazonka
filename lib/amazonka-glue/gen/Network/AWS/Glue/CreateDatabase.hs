{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in a Data Catalog.
module Network.AWS.Glue.CreateDatabase
  ( -- * Creating a request
    CreateDatabase (..),
    mkCreateDatabase,

    -- ** Request lenses
    cdDatabaseInput,
    cdCatalogId,

    -- * Destructuring the response
    CreateDatabaseResponse (..),
    mkCreateDatabaseResponse,

    -- ** Response lenses
    cdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { -- | The metadata for the database.
    databaseInput :: DatabaseInput,
    -- | The ID of the Data Catalog in which to create the database. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatabase' with the minimum fields required to make a request.
--
-- * 'databaseInput' - The metadata for the database.
-- * 'catalogId' - The ID of the Data Catalog in which to create the database. If none is provided, the AWS account ID is used by default.
mkCreateDatabase ::
  -- | 'databaseInput'
  DatabaseInput ->
  CreateDatabase
mkCreateDatabase pDatabaseInput_ =
  CreateDatabase'
    { databaseInput = pDatabaseInput_,
      catalogId = Lude.Nothing
    }

-- | The metadata for the database.
--
-- /Note:/ Consider using 'databaseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDatabaseInput :: Lens.Lens' CreateDatabase DatabaseInput
cdDatabaseInput = Lens.lens (databaseInput :: CreateDatabase -> DatabaseInput) (\s a -> s {databaseInput = a} :: CreateDatabase)
{-# DEPRECATED cdDatabaseInput "Use generic-lens or generic-optics with 'databaseInput' instead." #-}

-- | The ID of the Data Catalog in which to create the database. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCatalogId :: Lens.Lens' CreateDatabase (Lude.Maybe Lude.Text)
cdCatalogId = Lens.lens (catalogId :: CreateDatabase -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreateDatabase)
{-# DEPRECATED cdCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Lude.AWSRequest CreateDatabase where
  type Rs CreateDatabase = CreateDatabaseResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateDatabaseResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDatabase where
  toJSON CreateDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DatabaseInput" Lude..= databaseInput),
            ("CatalogId" Lude..=) Lude.<$> catalogId
          ]
      )

instance Lude.ToPath CreateDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDatabaseResponse' smart constructor.
newtype CreateDatabaseResponse = CreateDatabaseResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDatabaseResponse
mkCreateDatabaseResponse pResponseStatus_ =
  CreateDatabaseResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDatabaseResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDatabaseResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

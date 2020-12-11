{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Amazon Athena Data Catalog to AWS Glue
module Network.AWS.Glue.ImportCatalogToGlue
  ( -- * Creating a request
    ImportCatalogToGlue (..),
    mkImportCatalogToGlue,

    -- ** Request lenses
    ictgCatalogId,

    -- * Destructuring the response
    ImportCatalogToGlueResponse (..),
    mkImportCatalogToGlueResponse,

    -- ** Response lenses
    ictgrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportCatalogToGlue' smart constructor.
newtype ImportCatalogToGlue = ImportCatalogToGlue'
  { catalogId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportCatalogToGlue' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the catalog to import. Currently, this should be the AWS account ID.
mkImportCatalogToGlue ::
  ImportCatalogToGlue
mkImportCatalogToGlue =
  ImportCatalogToGlue' {catalogId = Lude.Nothing}

-- | The ID of the catalog to import. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgCatalogId :: Lens.Lens' ImportCatalogToGlue (Lude.Maybe Lude.Text)
ictgCatalogId = Lens.lens (catalogId :: ImportCatalogToGlue -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: ImportCatalogToGlue)
{-# DEPRECATED ictgCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Lude.AWSRequest ImportCatalogToGlue where
  type Rs ImportCatalogToGlue = ImportCatalogToGlueResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ImportCatalogToGlueResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportCatalogToGlue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ImportCatalogToGlue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportCatalogToGlue where
  toJSON ImportCatalogToGlue' {..} =
    Lude.object
      (Lude.catMaybes [("CatalogId" Lude..=) Lude.<$> catalogId])

instance Lude.ToPath ImportCatalogToGlue where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportCatalogToGlue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportCatalogToGlueResponse' smart constructor.
newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
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

-- | Creates a value of 'ImportCatalogToGlueResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkImportCatalogToGlueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportCatalogToGlueResponse
mkImportCatalogToGlueResponse pResponseStatus_ =
  ImportCatalogToGlueResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgrsResponseStatus :: Lens.Lens' ImportCatalogToGlueResponse Lude.Int
ictgrsResponseStatus = Lens.lens (responseStatus :: ImportCatalogToGlueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportCatalogToGlueResponse)
{-# DEPRECATED ictgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

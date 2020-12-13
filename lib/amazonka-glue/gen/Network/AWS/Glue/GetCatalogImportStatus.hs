{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a migration operation.
module Network.AWS.Glue.GetCatalogImportStatus
  ( -- * Creating a request
    GetCatalogImportStatus (..),
    mkGetCatalogImportStatus,

    -- ** Request lenses
    gcisCatalogId,

    -- * Destructuring the response
    GetCatalogImportStatusResponse (..),
    mkGetCatalogImportStatusResponse,

    -- ** Response lenses
    gcisrsImportStatus,
    gcisrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCatalogImportStatus' smart constructor.
newtype GetCatalogImportStatus = GetCatalogImportStatus'
  { -- | The ID of the catalog to migrate. Currently, this should be the AWS account ID.
    catalogId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCatalogImportStatus' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the catalog to migrate. Currently, this should be the AWS account ID.
mkGetCatalogImportStatus ::
  GetCatalogImportStatus
mkGetCatalogImportStatus =
  GetCatalogImportStatus' {catalogId = Lude.Nothing}

-- | The ID of the catalog to migrate. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisCatalogId :: Lens.Lens' GetCatalogImportStatus (Lude.Maybe Lude.Text)
gcisCatalogId = Lens.lens (catalogId :: GetCatalogImportStatus -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetCatalogImportStatus)
{-# DEPRECATED gcisCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Lude.AWSRequest GetCatalogImportStatus where
  type Rs GetCatalogImportStatus = GetCatalogImportStatusResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCatalogImportStatusResponse'
            Lude.<$> (x Lude..?> "ImportStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCatalogImportStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetCatalogImportStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCatalogImportStatus where
  toJSON GetCatalogImportStatus' {..} =
    Lude.object
      (Lude.catMaybes [("CatalogId" Lude..=) Lude.<$> catalogId])

instance Lude.ToPath GetCatalogImportStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCatalogImportStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCatalogImportStatusResponse' smart constructor.
data GetCatalogImportStatusResponse = GetCatalogImportStatusResponse'
  { -- | The status of the specified catalog migration.
    importStatus :: Lude.Maybe CatalogImportStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCatalogImportStatusResponse' with the minimum fields required to make a request.
--
-- * 'importStatus' - The status of the specified catalog migration.
-- * 'responseStatus' - The response status code.
mkGetCatalogImportStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCatalogImportStatusResponse
mkGetCatalogImportStatusResponse pResponseStatus_ =
  GetCatalogImportStatusResponse'
    { importStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the specified catalog migration.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrsImportStatus :: Lens.Lens' GetCatalogImportStatusResponse (Lude.Maybe CatalogImportStatus)
gcisrsImportStatus = Lens.lens (importStatus :: GetCatalogImportStatusResponse -> Lude.Maybe CatalogImportStatus) (\s a -> s {importStatus = a} :: GetCatalogImportStatusResponse)
{-# DEPRECATED gcisrsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrsResponseStatus :: Lens.Lens' GetCatalogImportStatusResponse Lude.Int
gcisrsResponseStatus = Lens.lens (responseStatus :: GetCatalogImportStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCatalogImportStatusResponse)
{-# DEPRECATED gcisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

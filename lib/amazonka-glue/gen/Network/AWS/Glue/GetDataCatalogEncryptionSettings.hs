{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configuration for a specified catalog.
module Network.AWS.Glue.GetDataCatalogEncryptionSettings
  ( -- * Creating a request
    GetDataCatalogEncryptionSettings (..),
    mkGetDataCatalogEncryptionSettings,

    -- ** Request lenses
    gdcesCatalogId,

    -- * Destructuring the response
    GetDataCatalogEncryptionSettingsResponse (..),
    mkGetDataCatalogEncryptionSettingsResponse,

    -- ** Response lenses
    gdcesrsDataCatalogEncryptionSettings,
    gdcesrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataCatalogEncryptionSettings' smart constructor.
newtype GetDataCatalogEncryptionSettings = GetDataCatalogEncryptionSettings'
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

-- | Creates a value of 'GetDataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog to retrieve the security configuration for. If none is provided, the AWS account ID is used by default.
mkGetDataCatalogEncryptionSettings ::
  GetDataCatalogEncryptionSettings
mkGetDataCatalogEncryptionSettings =
  GetDataCatalogEncryptionSettings' {catalogId = Lude.Nothing}

-- | The ID of the Data Catalog to retrieve the security configuration for. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcesCatalogId :: Lens.Lens' GetDataCatalogEncryptionSettings (Lude.Maybe Lude.Text)
gdcesCatalogId = Lens.lens (catalogId :: GetDataCatalogEncryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetDataCatalogEncryptionSettings)
{-# DEPRECATED gdcesCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Lude.AWSRequest GetDataCatalogEncryptionSettings where
  type
    Rs GetDataCatalogEncryptionSettings =
      GetDataCatalogEncryptionSettingsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataCatalogEncryptionSettingsResponse'
            Lude.<$> (x Lude..?> "DataCatalogEncryptionSettings")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataCatalogEncryptionSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetDataCatalogEncryptionSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDataCatalogEncryptionSettings where
  toJSON GetDataCatalogEncryptionSettings' {..} =
    Lude.object
      (Lude.catMaybes [("CatalogId" Lude..=) Lude.<$> catalogId])

instance Lude.ToPath GetDataCatalogEncryptionSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDataCatalogEncryptionSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDataCatalogEncryptionSettingsResponse' smart constructor.
data GetDataCatalogEncryptionSettingsResponse = GetDataCatalogEncryptionSettingsResponse'
  { dataCatalogEncryptionSettings ::
      Lude.Maybe
        DataCatalogEncryptionSettings,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataCatalogEncryptionSettingsResponse' with the minimum fields required to make a request.
--
-- * 'dataCatalogEncryptionSettings' - The requested security configuration.
-- * 'responseStatus' - The response status code.
mkGetDataCatalogEncryptionSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataCatalogEncryptionSettingsResponse
mkGetDataCatalogEncryptionSettingsResponse pResponseStatus_ =
  GetDataCatalogEncryptionSettingsResponse'
    { dataCatalogEncryptionSettings =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested security configuration.
--
-- /Note:/ Consider using 'dataCatalogEncryptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcesrsDataCatalogEncryptionSettings :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse (Lude.Maybe DataCatalogEncryptionSettings)
gdcesrsDataCatalogEncryptionSettings = Lens.lens (dataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> Lude.Maybe DataCatalogEncryptionSettings) (\s a -> s {dataCatalogEncryptionSettings = a} :: GetDataCatalogEncryptionSettingsResponse)
{-# DEPRECATED gdcesrsDataCatalogEncryptionSettings "Use generic-lens or generic-optics with 'dataCatalogEncryptionSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcesrsResponseStatus :: Lens.Lens' GetDataCatalogEncryptionSettingsResponse Lude.Int
gdcesrsResponseStatus = Lens.lens (responseStatus :: GetDataCatalogEncryptionSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataCatalogEncryptionSettingsResponse)
{-# DEPRECATED gdcesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

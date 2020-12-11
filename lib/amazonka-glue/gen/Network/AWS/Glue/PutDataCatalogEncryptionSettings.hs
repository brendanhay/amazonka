{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the security configuration for a specified catalog. After the configuration has been set, the specified encryption is applied to every catalog write thereafter.
module Network.AWS.Glue.PutDataCatalogEncryptionSettings
  ( -- * Creating a request
    PutDataCatalogEncryptionSettings (..),
    mkPutDataCatalogEncryptionSettings,

    -- ** Request lenses
    pdcesCatalogId,
    pdcesDataCatalogEncryptionSettings,

    -- * Destructuring the response
    PutDataCatalogEncryptionSettingsResponse (..),
    mkPutDataCatalogEncryptionSettingsResponse,

    -- ** Response lenses
    pdcesrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutDataCatalogEncryptionSettings' smart constructor.
data PutDataCatalogEncryptionSettings = PutDataCatalogEncryptionSettings'
  { catalogId ::
      Lude.Maybe Lude.Text,
    dataCatalogEncryptionSettings ::
      DataCatalogEncryptionSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
-- * 'dataCatalogEncryptionSettings' - The security configuration to set.
mkPutDataCatalogEncryptionSettings ::
  -- | 'dataCatalogEncryptionSettings'
  DataCatalogEncryptionSettings ->
  PutDataCatalogEncryptionSettings
mkPutDataCatalogEncryptionSettings pDataCatalogEncryptionSettings_ =
  PutDataCatalogEncryptionSettings'
    { catalogId = Lude.Nothing,
      dataCatalogEncryptionSettings =
        pDataCatalogEncryptionSettings_
    }

-- | The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesCatalogId :: Lens.Lens' PutDataCatalogEncryptionSettings (Lude.Maybe Lude.Text)
pdcesCatalogId = Lens.lens (catalogId :: PutDataCatalogEncryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: PutDataCatalogEncryptionSettings)
{-# DEPRECATED pdcesCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The security configuration to set.
--
-- /Note:/ Consider using 'dataCatalogEncryptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesDataCatalogEncryptionSettings :: Lens.Lens' PutDataCatalogEncryptionSettings DataCatalogEncryptionSettings
pdcesDataCatalogEncryptionSettings = Lens.lens (dataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettings -> DataCatalogEncryptionSettings) (\s a -> s {dataCatalogEncryptionSettings = a} :: PutDataCatalogEncryptionSettings)
{-# DEPRECATED pdcesDataCatalogEncryptionSettings "Use generic-lens or generic-optics with 'dataCatalogEncryptionSettings' instead." #-}

instance Lude.AWSRequest PutDataCatalogEncryptionSettings where
  type
    Rs PutDataCatalogEncryptionSettings =
      PutDataCatalogEncryptionSettingsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutDataCatalogEncryptionSettingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutDataCatalogEncryptionSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.PutDataCatalogEncryptionSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutDataCatalogEncryptionSettings where
  toJSON PutDataCatalogEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just
              ( "DataCatalogEncryptionSettings"
                  Lude..= dataCatalogEncryptionSettings
              )
          ]
      )

instance Lude.ToPath PutDataCatalogEncryptionSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery PutDataCatalogEncryptionSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutDataCatalogEncryptionSettingsResponse' smart constructor.
newtype PutDataCatalogEncryptionSettingsResponse = PutDataCatalogEncryptionSettingsResponse'
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

-- | Creates a value of 'PutDataCatalogEncryptionSettingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutDataCatalogEncryptionSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutDataCatalogEncryptionSettingsResponse
mkPutDataCatalogEncryptionSettingsResponse pResponseStatus_ =
  PutDataCatalogEncryptionSettingsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesrsResponseStatus :: Lens.Lens' PutDataCatalogEncryptionSettingsResponse Lude.Int
pdcesrsResponseStatus = Lens.lens (responseStatus :: PutDataCatalogEncryptionSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutDataCatalogEncryptionSettingsResponse)
{-# DEPRECATED pdcesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

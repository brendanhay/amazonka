{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified data catalog.
module Network.AWS.Athena.GetDataCatalog
  ( -- * Creating a request
    GetDataCatalog (..),
    mkGetDataCatalog,

    -- ** Request lenses
    gdcName,

    -- * Destructuring the response
    GetDataCatalogResponse (..),
    mkGetDataCatalogResponse,

    -- ** Response lenses
    gdcrsDataCatalog,
    gdcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataCatalog' smart constructor.
newtype GetDataCatalog = GetDataCatalog' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataCatalog' with the minimum fields required to make a request.
--
-- * 'name' - The name of the data catalog to return.
mkGetDataCatalog ::
  -- | 'name'
  Lude.Text ->
  GetDataCatalog
mkGetDataCatalog pName_ = GetDataCatalog' {name = pName_}

-- | The name of the data catalog to return.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcName :: Lens.Lens' GetDataCatalog Lude.Text
gdcName = Lens.lens (name :: GetDataCatalog -> Lude.Text) (\s a -> s {name = a} :: GetDataCatalog)
{-# DEPRECATED gdcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetDataCatalog where
  type Rs GetDataCatalog = GetDataCatalogResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataCatalogResponse'
            Lude.<$> (x Lude..?> "DataCatalog") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetDataCatalog" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDataCatalog where
  toJSON GetDataCatalog' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetDataCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDataCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDataCatalogResponse' smart constructor.
data GetDataCatalogResponse = GetDataCatalogResponse'
  { dataCatalog ::
      Lude.Maybe DataCatalog,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataCatalogResponse' with the minimum fields required to make a request.
--
-- * 'dataCatalog' - The data catalog returned.
-- * 'responseStatus' - The response status code.
mkGetDataCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataCatalogResponse
mkGetDataCatalogResponse pResponseStatus_ =
  GetDataCatalogResponse'
    { dataCatalog = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The data catalog returned.
--
-- /Note:/ Consider using 'dataCatalog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsDataCatalog :: Lens.Lens' GetDataCatalogResponse (Lude.Maybe DataCatalog)
gdcrsDataCatalog = Lens.lens (dataCatalog :: GetDataCatalogResponse -> Lude.Maybe DataCatalog) (\s a -> s {dataCatalog = a} :: GetDataCatalogResponse)
{-# DEPRECATED gdcrsDataCatalog "Use generic-lens or generic-optics with 'dataCatalog' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsResponseStatus :: Lens.Lens' GetDataCatalogResponse Lude.Int
gdcrsResponseStatus = Lens.lens (responseStatus :: GetDataCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataCatalogResponse)
{-# DEPRECATED gdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

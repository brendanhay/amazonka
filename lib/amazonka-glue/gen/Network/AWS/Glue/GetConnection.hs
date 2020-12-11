{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a connection definition from the Data Catalog.
module Network.AWS.Glue.GetConnection
  ( -- * Creating a request
    GetConnection (..),
    mkGetConnection,

    -- ** Request lenses
    gCatalogId,
    gHidePassword,
    gName,

    -- * Destructuring the response
    GetConnectionResponse (..),
    mkGetConnectionResponse,

    -- ** Response lenses
    gccrsConnection,
    gccrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnection' smart constructor.
data GetConnection = GetConnection'
  { catalogId ::
      Lude.Maybe Lude.Text,
    hidePassword :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'GetConnection' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
-- * 'hidePassword' - Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
-- * 'name' - The name of the connection definition to retrieve.
mkGetConnection ::
  -- | 'name'
  Lude.Text ->
  GetConnection
mkGetConnection pName_ =
  GetConnection'
    { catalogId = Lude.Nothing,
      hidePassword = Lude.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gCatalogId :: Lens.Lens' GetConnection (Lude.Maybe Lude.Text)
gCatalogId = Lens.lens (catalogId :: GetConnection -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetConnection)
{-# DEPRECATED gCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
--
-- /Note:/ Consider using 'hidePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gHidePassword :: Lens.Lens' GetConnection (Lude.Maybe Lude.Bool)
gHidePassword = Lens.lens (hidePassword :: GetConnection -> Lude.Maybe Lude.Bool) (\s a -> s {hidePassword = a} :: GetConnection)
{-# DEPRECATED gHidePassword "Use generic-lens or generic-optics with 'hidePassword' instead." #-}

-- | The name of the connection definition to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetConnection Lude.Text
gName = Lens.lens (name :: GetConnection -> Lude.Text) (\s a -> s {name = a} :: GetConnection)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetConnection where
  type Rs GetConnection = GetConnectionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Lude.<$> (x Lude..?> "Connection") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConnection where
  toJSON GetConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("HidePassword" Lude..=) Lude.<$> hidePassword,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { connection ::
      Lude.Maybe Connection,
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

-- | Creates a value of 'GetConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connection' - The requested connection definition.
-- * 'responseStatus' - The response status code.
mkGetConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectionResponse
mkGetConnectionResponse pResponseStatus_ =
  GetConnectionResponse'
    { connection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested connection definition.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsConnection :: Lens.Lens' GetConnectionResponse (Lude.Maybe Connection)
gccrsConnection = Lens.lens (connection :: GetConnectionResponse -> Lude.Maybe Connection) (\s a -> s {connection = a} :: GetConnectionResponse)
{-# DEPRECATED gccrsConnection "Use generic-lens or generic-optics with 'connection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsResponseStatus :: Lens.Lens' GetConnectionResponse Lude.Int
gccrsResponseStatus = Lens.lens (responseStatus :: GetConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectionResponse)
{-# DEPRECATED gccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

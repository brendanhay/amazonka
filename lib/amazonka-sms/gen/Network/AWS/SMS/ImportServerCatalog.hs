{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gathers a complete list of on-premises servers. Connectors must be installed and monitoring all servers to import.
--
-- This call returns immediately, but might take additional time to retrieve all the servers.
module Network.AWS.SMS.ImportServerCatalog
  ( -- * Creating a request
    ImportServerCatalog (..),
    mkImportServerCatalog,

    -- * Destructuring the response
    ImportServerCatalogResponse (..),
    mkImportServerCatalogResponse,

    -- ** Response lenses
    iscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkImportServerCatalog' smart constructor.
data ImportServerCatalog = ImportServerCatalog'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportServerCatalog' with the minimum fields required to make a request.
mkImportServerCatalog ::
  ImportServerCatalog
mkImportServerCatalog = ImportServerCatalog'

instance Lude.AWSRequest ImportServerCatalog where
  type Rs ImportServerCatalog = ImportServerCatalogResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ImportServerCatalogResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportServerCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.ImportServerCatalog" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportServerCatalog where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ImportServerCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportServerCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportServerCatalogResponse' smart constructor.
newtype ImportServerCatalogResponse = ImportServerCatalogResponse'
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

-- | Creates a value of 'ImportServerCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkImportServerCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportServerCatalogResponse
mkImportServerCatalogResponse pResponseStatus_ =
  ImportServerCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrsResponseStatus :: Lens.Lens' ImportServerCatalogResponse Lude.Int
iscrsResponseStatus = Lens.lens (responseStatus :: ImportServerCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportServerCatalogResponse)
{-# DEPRECATED iscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

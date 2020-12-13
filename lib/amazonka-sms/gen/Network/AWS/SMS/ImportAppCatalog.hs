{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ImportAppCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows application import from AWS Migration Hub.
module Network.AWS.SMS.ImportAppCatalog
  ( -- * Creating a request
    ImportAppCatalog (..),
    mkImportAppCatalog,

    -- ** Request lenses
    iacRoleName,

    -- * Destructuring the response
    ImportAppCatalogResponse (..),
    mkImportAppCatalogResponse,

    -- ** Response lenses
    iacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkImportAppCatalog' smart constructor.
newtype ImportAppCatalog = ImportAppCatalog'
  { -- | The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
    roleName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportAppCatalog' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
mkImportAppCatalog ::
  ImportAppCatalog
mkImportAppCatalog = ImportAppCatalog' {roleName = Lude.Nothing}

-- | The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iacRoleName :: Lens.Lens' ImportAppCatalog (Lude.Maybe Lude.Text)
iacRoleName = Lens.lens (roleName :: ImportAppCatalog -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ImportAppCatalog)
{-# DEPRECATED iacRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest ImportAppCatalog where
  type Rs ImportAppCatalog = ImportAppCatalogResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ImportAppCatalogResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportAppCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.ImportAppCatalog" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportAppCatalog where
  toJSON ImportAppCatalog' {..} =
    Lude.object
      (Lude.catMaybes [("roleName" Lude..=) Lude.<$> roleName])

instance Lude.ToPath ImportAppCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportAppCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportAppCatalogResponse' smart constructor.
newtype ImportAppCatalogResponse = ImportAppCatalogResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportAppCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkImportAppCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportAppCatalogResponse
mkImportAppCatalogResponse pResponseStatus_ =
  ImportAppCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iacrsResponseStatus :: Lens.Lens' ImportAppCatalogResponse Lude.Int
iacrsResponseStatus = Lens.lens (responseStatus :: ImportAppCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportAppCatalogResponse)
{-# DEPRECATED iacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

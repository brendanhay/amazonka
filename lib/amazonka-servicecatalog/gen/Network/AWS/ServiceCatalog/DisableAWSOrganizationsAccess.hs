{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable portfolio sharing through AWS Organizations feature. This feature will not delete your current shares but it will prevent you from creating new shares throughout your organization. Current shares will not be in sync with your organization structure if it changes after calling this API. This API can only be called by the management account in the organization.
--
-- This API can't be invoked if there are active delegated administrators in the organization.
-- Note that a delegated administrator is not authorized to invoke @DisableAWSOrganizationsAccess@ .
module Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
  ( -- * Creating a request
    DisableAWSOrganizationsAccess (..),
    mkDisableAWSOrganizationsAccess,

    -- * Destructuring the response
    DisableAWSOrganizationsAccessResponse (..),
    mkDisableAWSOrganizationsAccessResponse,

    -- ** Response lenses
    daoarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDisableAWSOrganizationsAccess' smart constructor.
data DisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAWSOrganizationsAccess' with the minimum fields required to make a request.
mkDisableAWSOrganizationsAccess ::
  DisableAWSOrganizationsAccess
mkDisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'

instance Lude.AWSRequest DisableAWSOrganizationsAccess where
  type
    Rs DisableAWSOrganizationsAccess =
      DisableAWSOrganizationsAccessResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableAWSOrganizationsAccessResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableAWSOrganizationsAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DisableAWSOrganizationsAccess" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableAWSOrganizationsAccess where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisableAWSOrganizationsAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableAWSOrganizationsAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableAWSOrganizationsAccessResponse' smart constructor.
newtype DisableAWSOrganizationsAccessResponse = DisableAWSOrganizationsAccessResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAWSOrganizationsAccessResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableAWSOrganizationsAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableAWSOrganizationsAccessResponse
mkDisableAWSOrganizationsAccessResponse pResponseStatus_ =
  DisableAWSOrganizationsAccessResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoarsResponseStatus :: Lens.Lens' DisableAWSOrganizationsAccessResponse Lude.Int
daoarsResponseStatus = Lens.lens (responseStatus :: DisableAWSOrganizationsAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableAWSOrganizationsAccessResponse)
{-# DEPRECATED daoarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

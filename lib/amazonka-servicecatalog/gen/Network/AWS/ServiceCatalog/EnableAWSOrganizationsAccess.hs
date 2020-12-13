{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable portfolio sharing feature through AWS Organizations. This API will allow Service Catalog to receive updates on your organization in order to sync your shares with the current structure. This API can only be called by the management account in the organization.
--
-- By calling this API Service Catalog will make a call to organizations:EnableAWSServiceAccess on your behalf so that your shares can be in sync with any changes in your AWS Organizations structure.
-- Note that a delegated administrator is not authorized to invoke @EnableAWSOrganizationsAccess@ .
module Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
  ( -- * Creating a request
    EnableAWSOrganizationsAccess (..),
    mkEnableAWSOrganizationsAccess,

    -- * Destructuring the response
    EnableAWSOrganizationsAccessResponse (..),
    mkEnableAWSOrganizationsAccessResponse,

    -- ** Response lenses
    eaoarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkEnableAWSOrganizationsAccess' smart constructor.
data EnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAWSOrganizationsAccess' with the minimum fields required to make a request.
mkEnableAWSOrganizationsAccess ::
  EnableAWSOrganizationsAccess
mkEnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'

instance Lude.AWSRequest EnableAWSOrganizationsAccess where
  type
    Rs EnableAWSOrganizationsAccess =
      EnableAWSOrganizationsAccessResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableAWSOrganizationsAccessResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableAWSOrganizationsAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.EnableAWSOrganizationsAccess" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableAWSOrganizationsAccess where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableAWSOrganizationsAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAWSOrganizationsAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableAWSOrganizationsAccessResponse' smart constructor.
newtype EnableAWSOrganizationsAccessResponse = EnableAWSOrganizationsAccessResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAWSOrganizationsAccessResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableAWSOrganizationsAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableAWSOrganizationsAccessResponse
mkEnableAWSOrganizationsAccessResponse pResponseStatus_ =
  EnableAWSOrganizationsAccessResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoarsResponseStatus :: Lens.Lens' EnableAWSOrganizationsAccessResponse Lude.Int
eaoarsResponseStatus = Lens.lens (responseStatus :: EnableAWSOrganizationsAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableAWSOrganizationsAccessResponse)
{-# DEPRECATED eaoarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for AWS Organization portfolio share feature. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
  ( -- * Creating a request
    GetAWSOrganizationsAccessStatus (..),
    mkGetAWSOrganizationsAccessStatus,

    -- * Destructuring the response
    GetAWSOrganizationsAccessStatusResponse (..),
    mkGetAWSOrganizationsAccessStatusResponse,

    -- ** Response lenses
    gaoasrsAccessStatus,
    gaoasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkGetAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAWSOrganizationsAccessStatus' with the minimum fields required to make a request.
mkGetAWSOrganizationsAccessStatus ::
  GetAWSOrganizationsAccessStatus
mkGetAWSOrganizationsAccessStatus =
  GetAWSOrganizationsAccessStatus'

instance Lude.AWSRequest GetAWSOrganizationsAccessStatus where
  type
    Rs GetAWSOrganizationsAccessStatus =
      GetAWSOrganizationsAccessStatusResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAWSOrganizationsAccessStatusResponse'
            Lude.<$> (x Lude..?> "AccessStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAWSOrganizationsAccessStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAWSOrganizationsAccessStatus where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetAWSOrganizationsAccessStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAWSOrganizationsAccessStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { -- | The status of the portfolio share feature.
    accessStatus :: Lude.Maybe AccessStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAWSOrganizationsAccessStatusResponse' with the minimum fields required to make a request.
--
-- * 'accessStatus' - The status of the portfolio share feature.
-- * 'responseStatus' - The response status code.
mkGetAWSOrganizationsAccessStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAWSOrganizationsAccessStatusResponse
mkGetAWSOrganizationsAccessStatusResponse pResponseStatus_ =
  GetAWSOrganizationsAccessStatusResponse'
    { accessStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the portfolio share feature.
--
-- /Note:/ Consider using 'accessStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaoasrsAccessStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse (Lude.Maybe AccessStatus)
gaoasrsAccessStatus = Lens.lens (accessStatus :: GetAWSOrganizationsAccessStatusResponse -> Lude.Maybe AccessStatus) (\s a -> s {accessStatus = a} :: GetAWSOrganizationsAccessStatusResponse)
{-# DEPRECATED gaoasrsAccessStatus "Use generic-lens or generic-optics with 'accessStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaoasrsResponseStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse Lude.Int
gaoasrsResponseStatus = Lens.lens (responseStatus :: GetAWSOrganizationsAccessStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAWSOrganizationsAccessStatusResponse)
{-# DEPRECATED gaoasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

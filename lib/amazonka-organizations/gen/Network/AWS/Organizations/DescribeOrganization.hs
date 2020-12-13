{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the organization that the user's account belongs to.
--
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeOrganization
  ( -- * Creating a request
    DescribeOrganization (..),
    mkDescribeOrganization,

    -- * Destructuring the response
    DescribeOrganizationResponse (..),
    mkDescribeOrganizationResponse,

    -- ** Response lenses
    dorsOrganization,
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganization' smart constructor.
data DescribeOrganization = DescribeOrganization'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganization' with the minimum fields required to make a request.
mkDescribeOrganization ::
  DescribeOrganization
mkDescribeOrganization = DescribeOrganization'

instance Lude.AWSRequest DescribeOrganization where
  type Rs DescribeOrganization = DescribeOrganizationResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Lude.<$> (x Lude..?> "Organization") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DescribeOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganization where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { -- | A structure that contains information about the organization.
    --
    -- /Important:/ The @AvailablePolicyTypes@ part of the response is deprecated, and you shouldn't use it in your apps. It doesn't include any policy type supported by Organizations other than SCPs. To determine which policy types are enabled in your organization, use the @'ListRoots' @ operation.
    organization :: Lude.Maybe Organization,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'organization' - A structure that contains information about the organization.
--
-- /Important:/ The @AvailablePolicyTypes@ part of the response is deprecated, and you shouldn't use it in your apps. It doesn't include any policy type supported by Organizations other than SCPs. To determine which policy types are enabled in your organization, use the @'ListRoots' @ operation.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationResponse
mkDescribeOrganizationResponse pResponseStatus_ =
  DescribeOrganizationResponse'
    { organization = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains information about the organization.
--
-- /Important:/ The @AvailablePolicyTypes@ part of the response is deprecated, and you shouldn't use it in your apps. It doesn't include any policy type supported by Organizations other than SCPs. To determine which policy types are enabled in your organization, use the @'ListRoots' @ operation.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsOrganization :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Organization)
dorsOrganization = Lens.lens (organization :: DescribeOrganizationResponse -> Lude.Maybe Organization) (\s a -> s {organization = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsOrganization "Use generic-lens or generic-optics with 'organization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DescribeOrganizationResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

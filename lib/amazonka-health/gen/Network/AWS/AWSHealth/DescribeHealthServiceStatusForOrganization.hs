{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation provides status information on enabling or disabling AWS Health to work with your organization. To call this operation, you must sign in as an IAM user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master account.
module Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
  ( -- * Creating a request
    DescribeHealthServiceStatusForOrganization (..),
    mkDescribeHealthServiceStatusForOrganization,

    -- * Destructuring the response
    DescribeHealthServiceStatusForOrganizationResponse (..),
    mkDescribeHealthServiceStatusForOrganizationResponse,

    -- ** Response lenses
    dhssforsHealthServiceAccessStatusForOrganization,
    dhssforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHealthServiceStatusForOrganization' smart constructor.
data DescribeHealthServiceStatusForOrganization = DescribeHealthServiceStatusForOrganization'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHealthServiceStatusForOrganization' with the minimum fields required to make a request.
mkDescribeHealthServiceStatusForOrganization ::
  DescribeHealthServiceStatusForOrganization
mkDescribeHealthServiceStatusForOrganization =
  DescribeHealthServiceStatusForOrganization'

instance Lude.AWSRequest DescribeHealthServiceStatusForOrganization where
  type
    Rs DescribeHealthServiceStatusForOrganization =
      DescribeHealthServiceStatusForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHealthServiceStatusForOrganizationResponse'
            Lude.<$> (x Lude..?> "healthServiceAccessStatusForOrganization")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHealthServiceStatusForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DescribeHealthServiceStatusForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHealthServiceStatusForOrganization where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeHealthServiceStatusForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHealthServiceStatusForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeHealthServiceStatusForOrganizationResponse' smart constructor.
data DescribeHealthServiceStatusForOrganizationResponse = DescribeHealthServiceStatusForOrganizationResponse'
  { -- | Information about the status of enabling or disabling AWS Health Organizational View in your organization.
    --
    -- Valid values are @ENABLED | DISABLED | PENDING@ .
    healthServiceAccessStatusForOrganization :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHealthServiceStatusForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'healthServiceAccessStatusForOrganization' - Information about the status of enabling or disabling AWS Health Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@ .
-- * 'responseStatus' - The response status code.
mkDescribeHealthServiceStatusForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHealthServiceStatusForOrganizationResponse
mkDescribeHealthServiceStatusForOrganizationResponse
  pResponseStatus_ =
    DescribeHealthServiceStatusForOrganizationResponse'
      { healthServiceAccessStatusForOrganization =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the status of enabling or disabling AWS Health Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@ .
--
-- /Note:/ Consider using 'healthServiceAccessStatusForOrganization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhssforsHealthServiceAccessStatusForOrganization :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse (Lude.Maybe Lude.Text)
dhssforsHealthServiceAccessStatusForOrganization = Lens.lens (healthServiceAccessStatusForOrganization :: DescribeHealthServiceStatusForOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {healthServiceAccessStatusForOrganization = a} :: DescribeHealthServiceStatusForOrganizationResponse)
{-# DEPRECATED dhssforsHealthServiceAccessStatusForOrganization "Use generic-lens or generic-optics with 'healthServiceAccessStatusForOrganization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhssforsResponseStatus :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse Lude.Int
dhssforsResponseStatus = Lens.lens (responseStatus :: DescribeHealthServiceStatusForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHealthServiceStatusForOrganizationResponse)
{-# DEPRECATED dhssforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

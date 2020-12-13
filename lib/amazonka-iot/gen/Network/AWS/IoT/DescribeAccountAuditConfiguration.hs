{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.DescribeAccountAuditConfiguration
  ( -- * Creating a request
    DescribeAccountAuditConfiguration (..),
    mkDescribeAccountAuditConfiguration,

    -- * Destructuring the response
    DescribeAccountAuditConfigurationResponse (..),
    mkDescribeAccountAuditConfigurationResponse,

    -- ** Response lenses
    daacrsAuditCheckConfigurations,
    daacrsAuditNotificationTargetConfigurations,
    daacrsRoleARN,
    daacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountAuditConfiguration' smart constructor.
data DescribeAccountAuditConfiguration = DescribeAccountAuditConfiguration'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAuditConfiguration' with the minimum fields required to make a request.
mkDescribeAccountAuditConfiguration ::
  DescribeAccountAuditConfiguration
mkDescribeAccountAuditConfiguration =
  DescribeAccountAuditConfiguration'

instance Lude.AWSRequest DescribeAccountAuditConfiguration where
  type
    Rs DescribeAccountAuditConfiguration =
      DescribeAccountAuditConfigurationResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountAuditConfigurationResponse'
            Lude.<$> (x Lude..?> "auditCheckConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> ( x Lude..?> "auditNotificationTargetConfigurations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAuditConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountAuditConfiguration where
  toPath = Lude.const "/audit/configuration"

instance Lude.ToQuery DescribeAccountAuditConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAccountAuditConfigurationResponse' smart constructor.
data DescribeAccountAuditConfigurationResponse = DescribeAccountAuditConfigurationResponse'
  { -- | Which audit checks are enabled and disabled for this account.
    auditCheckConfigurations :: Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration)),
    -- | Information about the targets to which audit notifications are sent for this account.
    auditNotificationTargetConfigurations :: Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget)),
    -- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit.
    --
    -- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'auditCheckConfigurations' - Which audit checks are enabled and disabled for this account.
-- * 'auditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent for this account.
-- * 'roleARN' - The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAuditConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAuditConfigurationResponse
mkDescribeAccountAuditConfigurationResponse pResponseStatus_ =
  DescribeAccountAuditConfigurationResponse'
    { auditCheckConfigurations =
        Lude.Nothing,
      auditNotificationTargetConfigurations = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Which audit checks are enabled and disabled for this account.
--
-- /Note:/ Consider using 'auditCheckConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrsAuditCheckConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration)))
daacrsAuditCheckConfigurations = Lens.lens (auditCheckConfigurations :: DescribeAccountAuditConfigurationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration))) (\s a -> s {auditCheckConfigurations = a} :: DescribeAccountAuditConfigurationResponse)
{-# DEPRECATED daacrsAuditCheckConfigurations "Use generic-lens or generic-optics with 'auditCheckConfigurations' instead." #-}

-- | Information about the targets to which audit notifications are sent for this account.
--
-- /Note:/ Consider using 'auditNotificationTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrsAuditNotificationTargetConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget)))
daacrsAuditNotificationTargetConfigurations = Lens.lens (auditNotificationTargetConfigurations :: DescribeAccountAuditConfigurationResponse -> Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget))) (\s a -> s {auditNotificationTargetConfigurations = a} :: DescribeAccountAuditConfigurationResponse)
{-# DEPRECATED daacrsAuditNotificationTargetConfigurations "Use generic-lens or generic-optics with 'auditNotificationTargetConfigurations' instead." #-}

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrsRoleARN :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Lude.Maybe Lude.Text)
daacrsRoleARN = Lens.lens (roleARN :: DescribeAccountAuditConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeAccountAuditConfigurationResponse)
{-# DEPRECATED daacrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacrsResponseStatus :: Lens.Lens' DescribeAccountAuditConfigurationResponse Lude.Int
daacrsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAuditConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAuditConfigurationResponse)
{-# DEPRECATED daacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

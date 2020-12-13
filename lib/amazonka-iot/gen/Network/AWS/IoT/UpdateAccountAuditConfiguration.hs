{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures or reconfigures the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.UpdateAccountAuditConfiguration
  ( -- * Creating a request
    UpdateAccountAuditConfiguration (..),
    mkUpdateAccountAuditConfiguration,

    -- ** Request lenses
    uaacAuditCheckConfigurations,
    uaacAuditNotificationTargetConfigurations,
    uaacRoleARN,

    -- * Destructuring the response
    UpdateAccountAuditConfigurationResponse (..),
    mkUpdateAccountAuditConfigurationResponse,

    -- ** Response lenses
    uaacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAccountAuditConfiguration' smart constructor.
data UpdateAccountAuditConfiguration = UpdateAccountAuditConfiguration'
  { -- | Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled.
    --
    -- Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted.
    -- You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself.
    -- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
    auditCheckConfigurations :: Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration)),
    -- | Information about the targets to which audit notifications are sent.
    auditNotificationTargetConfigurations :: Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget)),
    -- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountAuditConfiguration' with the minimum fields required to make a request.
--
-- * 'auditCheckConfigurations' - Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted.
-- You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself.
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
-- * 'auditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent.
-- * 'roleARN' - The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
mkUpdateAccountAuditConfiguration ::
  UpdateAccountAuditConfiguration
mkUpdateAccountAuditConfiguration =
  UpdateAccountAuditConfiguration'
    { auditCheckConfigurations =
        Lude.Nothing,
      auditNotificationTargetConfigurations = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted.
-- You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself.
-- On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
--
-- /Note:/ Consider using 'auditCheckConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacAuditCheckConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration)))
uaacAuditCheckConfigurations = Lens.lens (auditCheckConfigurations :: UpdateAccountAuditConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckConfiguration))) (\s a -> s {auditCheckConfigurations = a} :: UpdateAccountAuditConfiguration)
{-# DEPRECATED uaacAuditCheckConfigurations "Use generic-lens or generic-optics with 'auditCheckConfigurations' instead." #-}

-- | Information about the targets to which audit notifications are sent.
--
-- /Note:/ Consider using 'auditNotificationTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacAuditNotificationTargetConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget)))
uaacAuditNotificationTargetConfigurations = Lens.lens (auditNotificationTargetConfigurations :: UpdateAccountAuditConfiguration -> Lude.Maybe (Lude.HashMap AuditNotificationType (AuditNotificationTarget))) (\s a -> s {auditNotificationTargetConfigurations = a} :: UpdateAccountAuditConfiguration)
{-# DEPRECATED uaacAuditNotificationTargetConfigurations "Use generic-lens or generic-optics with 'auditNotificationTargetConfigurations' instead." #-}

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacRoleARN :: Lens.Lens' UpdateAccountAuditConfiguration (Lude.Maybe Lude.Text)
uaacRoleARN = Lens.lens (roleARN :: UpdateAccountAuditConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateAccountAuditConfiguration)
{-# DEPRECATED uaacRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateAccountAuditConfiguration where
  type
    Rs UpdateAccountAuditConfiguration =
      UpdateAccountAuditConfigurationResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateAccountAuditConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAccountAuditConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateAccountAuditConfiguration where
  toJSON UpdateAccountAuditConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("auditCheckConfigurations" Lude..=)
              Lude.<$> auditCheckConfigurations,
            ("auditNotificationTargetConfigurations" Lude..=)
              Lude.<$> auditNotificationTargetConfigurations,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateAccountAuditConfiguration where
  toPath = Lude.const "/audit/configuration"

instance Lude.ToQuery UpdateAccountAuditConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAccountAuditConfigurationResponse' smart constructor.
newtype UpdateAccountAuditConfigurationResponse = UpdateAccountAuditConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateAccountAuditConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAccountAuditConfigurationResponse
mkUpdateAccountAuditConfigurationResponse pResponseStatus_ =
  UpdateAccountAuditConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaacrsResponseStatus :: Lens.Lens' UpdateAccountAuditConfigurationResponse Lude.Int
uaacrsResponseStatus = Lens.lens (responseStatus :: UpdateAccountAuditConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAccountAuditConfigurationResponse)
{-# DEPRECATED uaacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

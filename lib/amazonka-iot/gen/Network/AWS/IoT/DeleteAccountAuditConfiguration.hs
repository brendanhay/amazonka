{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the default settings for Device Defender audits for this account. Any configuration data you entered is deleted and all audit checks are reset to disabled.
module Network.AWS.IoT.DeleteAccountAuditConfiguration
  ( -- * Creating a request
    DeleteAccountAuditConfiguration (..),
    mkDeleteAccountAuditConfiguration,

    -- ** Request lenses
    daacDeleteScheduledAudits,

    -- * Destructuring the response
    DeleteAccountAuditConfigurationResponse (..),
    mkDeleteAccountAuditConfigurationResponse,

    -- ** Response lenses
    daacfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccountAuditConfiguration' smart constructor.
newtype DeleteAccountAuditConfiguration = DeleteAccountAuditConfiguration'
  { -- | If true, all scheduled audits are deleted.
    deleteScheduledAudits :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountAuditConfiguration' with the minimum fields required to make a request.
--
-- * 'deleteScheduledAudits' - If true, all scheduled audits are deleted.
mkDeleteAccountAuditConfiguration ::
  DeleteAccountAuditConfiguration
mkDeleteAccountAuditConfiguration =
  DeleteAccountAuditConfiguration'
    { deleteScheduledAudits =
        Lude.Nothing
    }

-- | If true, all scheduled audits are deleted.
--
-- /Note:/ Consider using 'deleteScheduledAudits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacDeleteScheduledAudits :: Lens.Lens' DeleteAccountAuditConfiguration (Lude.Maybe Lude.Bool)
daacDeleteScheduledAudits = Lens.lens (deleteScheduledAudits :: DeleteAccountAuditConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {deleteScheduledAudits = a} :: DeleteAccountAuditConfiguration)
{-# DEPRECATED daacDeleteScheduledAudits "Use generic-lens or generic-optics with 'deleteScheduledAudits' instead." #-}

instance Lude.AWSRequest DeleteAccountAuditConfiguration where
  type
    Rs DeleteAccountAuditConfiguration =
      DeleteAccountAuditConfigurationResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAccountAuditConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAccountAuditConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAccountAuditConfiguration where
  toPath = Lude.const "/audit/configuration"

instance Lude.ToQuery DeleteAccountAuditConfiguration where
  toQuery DeleteAccountAuditConfiguration' {..} =
    Lude.mconcat
      ["deleteScheduledAudits" Lude.=: deleteScheduledAudits]

-- | /See:/ 'mkDeleteAccountAuditConfigurationResponse' smart constructor.
newtype DeleteAccountAuditConfigurationResponse = DeleteAccountAuditConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAccountAuditConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAccountAuditConfigurationResponse
mkDeleteAccountAuditConfigurationResponse pResponseStatus_ =
  DeleteAccountAuditConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daacfrsResponseStatus :: Lens.Lens' DeleteAccountAuditConfigurationResponse Lude.Int
daacfrsResponseStatus = Lens.lens (responseStatus :: DeleteAccountAuditConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAccountAuditConfigurationResponse)
{-# DEPRECATED daacfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

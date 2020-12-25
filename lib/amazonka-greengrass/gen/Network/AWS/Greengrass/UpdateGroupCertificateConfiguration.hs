{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Certificate expiry time for a group.
module Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
  ( -- * Creating a request
    UpdateGroupCertificateConfiguration (..),
    mkUpdateGroupCertificateConfiguration,

    -- ** Request lenses
    ugccGroupId,
    ugccCertificateExpiryInMilliseconds,

    -- * Destructuring the response
    UpdateGroupCertificateConfigurationResponse (..),
    mkUpdateGroupCertificateConfigurationResponse,

    -- ** Response lenses
    ugccrrsCertificateAuthorityExpiryInMilliseconds,
    ugccrrsCertificateExpiryInMilliseconds,
    ugccrrsGroupId,
    ugccrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The amount of time remaining before the certificate expires, in milliseconds.
    certificateExpiryInMilliseconds :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupCertificateConfiguration' value with any optional fields omitted.
mkUpdateGroupCertificateConfiguration ::
  -- | 'groupId'
  Core.Text ->
  UpdateGroupCertificateConfiguration
mkUpdateGroupCertificateConfiguration groupId =
  UpdateGroupCertificateConfiguration'
    { groupId,
      certificateExpiryInMilliseconds = Core.Nothing
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccGroupId :: Lens.Lens' UpdateGroupCertificateConfiguration Core.Text
ugccGroupId = Lens.field @"groupId"
{-# DEPRECATED ugccGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The amount of time remaining before the certificate expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccCertificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfiguration (Core.Maybe Core.Text)
ugccCertificateExpiryInMilliseconds = Lens.field @"certificateExpiryInMilliseconds"
{-# DEPRECATED ugccCertificateExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateExpiryInMilliseconds' instead." #-}

instance Core.FromJSON UpdateGroupCertificateConfiguration where
  toJSON UpdateGroupCertificateConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("CertificateExpiryInMilliseconds" Core..=)
              Core.<$> certificateExpiryInMilliseconds
          ]
      )

instance Core.AWSRequest UpdateGroupCertificateConfiguration where
  type
    Rs UpdateGroupCertificateConfiguration =
      UpdateGroupCertificateConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/certificateauthorities/configuration/expiry")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupCertificateConfigurationResponse'
            Core.<$> (x Core..:? "CertificateAuthorityExpiryInMilliseconds")
            Core.<*> (x Core..:? "CertificateExpiryInMilliseconds")
            Core.<*> (x Core..:? "GroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
  { -- | The amount of time remaining before the certificate authority expires, in milliseconds.
    certificateAuthorityExpiryInMilliseconds :: Core.Maybe Core.Text,
    -- | The amount of time remaining before the certificate expires, in milliseconds.
    certificateExpiryInMilliseconds :: Core.Maybe Core.Text,
    -- | The ID of the group certificate configuration.
    groupId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupCertificateConfigurationResponse' value with any optional fields omitted.
mkUpdateGroupCertificateConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGroupCertificateConfigurationResponse
mkUpdateGroupCertificateConfigurationResponse responseStatus =
  UpdateGroupCertificateConfigurationResponse'
    { certificateAuthorityExpiryInMilliseconds =
        Core.Nothing,
      certificateExpiryInMilliseconds = Core.Nothing,
      groupId = Core.Nothing,
      responseStatus
    }

-- | The amount of time remaining before the certificate authority expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateAuthorityExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrrsCertificateAuthorityExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
ugccrrsCertificateAuthorityExpiryInMilliseconds = Lens.field @"certificateAuthorityExpiryInMilliseconds"
{-# DEPRECATED ugccrrsCertificateAuthorityExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateAuthorityExpiryInMilliseconds' instead." #-}

-- | The amount of time remaining before the certificate expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrrsCertificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
ugccrrsCertificateExpiryInMilliseconds = Lens.field @"certificateExpiryInMilliseconds"
{-# DEPRECATED ugccrrsCertificateExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateExpiryInMilliseconds' instead." #-}

-- | The ID of the group certificate configuration.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrrsGroupId :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
ugccrrsGroupId = Lens.field @"groupId"
{-# DEPRECATED ugccrrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrrsResponseStatus :: Lens.Lens' UpdateGroupCertificateConfigurationResponse Core.Int
ugccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

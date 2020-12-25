{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Device Defender security profile with a thing group or this account. Each thing group or account can have up to five security profiles associated with it.
module Network.AWS.IoT.AttachSecurityProfile
  ( -- * Creating a request
    AttachSecurityProfile (..),
    mkAttachSecurityProfile,

    -- ** Request lenses
    aspSecurityProfileName,
    aspSecurityProfileTargetArn,

    -- * Destructuring the response
    AttachSecurityProfileResponse (..),
    mkAttachSecurityProfileResponse,

    -- ** Response lenses
    asprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachSecurityProfile' smart constructor.
data AttachSecurityProfile = AttachSecurityProfile'
  { -- | The security profile that is attached.
    securityProfileName :: Types.SecurityProfileName,
    -- | The ARN of the target (thing group) to which the security profile is attached.
    securityProfileTargetArn :: Types.SecurityProfileTargetArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachSecurityProfile' value with any optional fields omitted.
mkAttachSecurityProfile ::
  -- | 'securityProfileName'
  Types.SecurityProfileName ->
  -- | 'securityProfileTargetArn'
  Types.SecurityProfileTargetArn ->
  AttachSecurityProfile
mkAttachSecurityProfile
  securityProfileName
  securityProfileTargetArn =
    AttachSecurityProfile'
      { securityProfileName,
        securityProfileTargetArn
      }

-- | The security profile that is attached.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspSecurityProfileName :: Lens.Lens' AttachSecurityProfile Types.SecurityProfileName
aspSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED aspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The ARN of the target (thing group) to which the security profile is attached.
--
-- /Note:/ Consider using 'securityProfileTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspSecurityProfileTargetArn :: Lens.Lens' AttachSecurityProfile Types.SecurityProfileTargetArn
aspSecurityProfileTargetArn = Lens.field @"securityProfileTargetArn"
{-# DEPRECATED aspSecurityProfileTargetArn "Use generic-lens or generic-optics with 'securityProfileTargetArn' instead." #-}

instance Core.FromJSON AttachSecurityProfile where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest AttachSecurityProfile where
  type Rs AttachSecurityProfile = AttachSecurityProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/security-profiles/" Core.<> (Core.toText securityProfileName)
                Core.<> ("/targets")
            ),
        Core._rqQuery =
          Core.toQueryValue
            "securityProfileTargetArn"
            securityProfileTargetArn,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachSecurityProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachSecurityProfileResponse' smart constructor.
newtype AttachSecurityProfileResponse = AttachSecurityProfileResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachSecurityProfileResponse' value with any optional fields omitted.
mkAttachSecurityProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachSecurityProfileResponse
mkAttachSecurityProfileResponse responseStatus =
  AttachSecurityProfileResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asprrsResponseStatus :: Lens.Lens' AttachSecurityProfileResponse Core.Int
asprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

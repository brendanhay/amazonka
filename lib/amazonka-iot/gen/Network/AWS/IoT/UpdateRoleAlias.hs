{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a role alias.
module Network.AWS.IoT.UpdateRoleAlias
  ( -- * Creating a request
    UpdateRoleAlias (..),
    mkUpdateRoleAlias,

    -- ** Request lenses
    uraRoleAlias,
    uraCredentialDurationSeconds,
    uraRoleArn,

    -- * Destructuring the response
    UpdateRoleAliasResponse (..),
    mkUpdateRoleAliasResponse,

    -- ** Response lenses
    urarrsRoleAlias,
    urarrsRoleAliasArn,
    urarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoleAlias' smart constructor.
data UpdateRoleAlias = UpdateRoleAlias'
  { -- | The role alias to update.
    roleAlias :: Types.RoleAlias,
    -- | The number of seconds the credential will be valid.
    credentialDurationSeconds :: Core.Maybe Core.Natural,
    -- | The role ARN.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoleAlias' value with any optional fields omitted.
mkUpdateRoleAlias ::
  -- | 'roleAlias'
  Types.RoleAlias ->
  UpdateRoleAlias
mkUpdateRoleAlias roleAlias =
  UpdateRoleAlias'
    { roleAlias,
      credentialDurationSeconds = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The role alias to update.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRoleAlias :: Lens.Lens' UpdateRoleAlias Types.RoleAlias
uraRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED uraRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The number of seconds the credential will be valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraCredentialDurationSeconds :: Lens.Lens' UpdateRoleAlias (Core.Maybe Core.Natural)
uraCredentialDurationSeconds = Lens.field @"credentialDurationSeconds"
{-# DEPRECATED uraCredentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRoleArn :: Lens.Lens' UpdateRoleAlias (Core.Maybe Types.RoleArn)
uraRoleArn = Lens.field @"roleArn"
{-# DEPRECATED uraRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON UpdateRoleAlias where
  toJSON UpdateRoleAlias {..} =
    Core.object
      ( Core.catMaybes
          [ ("credentialDurationSeconds" Core..=)
              Core.<$> credentialDurationSeconds,
            ("roleArn" Core..=) Core.<$> roleArn
          ]
      )

instance Core.AWSRequest UpdateRoleAlias where
  type Rs UpdateRoleAlias = UpdateRoleAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/role-aliases/" Core.<> (Core.toText roleAlias)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoleAliasResponse'
            Core.<$> (x Core..:? "roleAlias")
            Core.<*> (x Core..:? "roleAliasArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRoleAliasResponse' smart constructor.
data UpdateRoleAliasResponse = UpdateRoleAliasResponse'
  { -- | The role alias.
    roleAlias :: Core.Maybe Types.RoleAlias,
    -- | The role alias ARN.
    roleAliasArn :: Core.Maybe Types.RoleAliasArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoleAliasResponse' value with any optional fields omitted.
mkUpdateRoleAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRoleAliasResponse
mkUpdateRoleAliasResponse responseStatus =
  UpdateRoleAliasResponse'
    { roleAlias = Core.Nothing,
      roleAliasArn = Core.Nothing,
      responseStatus
    }

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarrsRoleAlias :: Lens.Lens' UpdateRoleAliasResponse (Core.Maybe Types.RoleAlias)
urarrsRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED urarrsRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The role alias ARN.
--
-- /Note:/ Consider using 'roleAliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarrsRoleAliasArn :: Lens.Lens' UpdateRoleAliasResponse (Core.Maybe Types.RoleAliasArn)
urarrsRoleAliasArn = Lens.field @"roleAliasArn"
{-# DEPRECATED urarrsRoleAliasArn "Use generic-lens or generic-optics with 'roleAliasArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarrsResponseStatus :: Lens.Lens' UpdateRoleAliasResponse Core.Int
urarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

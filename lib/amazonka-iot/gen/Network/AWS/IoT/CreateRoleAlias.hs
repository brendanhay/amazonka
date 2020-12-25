{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a role alias.
module Network.AWS.IoT.CreateRoleAlias
  ( -- * Creating a request
    CreateRoleAlias (..),
    mkCreateRoleAlias,

    -- ** Request lenses
    craRoleAlias,
    craRoleArn,
    craCredentialDurationSeconds,
    craTags,

    -- * Destructuring the response
    CreateRoleAliasResponse (..),
    mkCreateRoleAliasResponse,

    -- ** Response lenses
    crarrsRoleAlias,
    crarrsRoleAliasArn,
    crarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { -- | The role alias that points to a role ARN. This allows you to change the role without having to update the device.
    roleAlias :: Types.RoleAlias,
    -- | The role ARN.
    roleArn :: Types.RoleArn,
    -- | How long (in seconds) the credentials will be valid.
    credentialDurationSeconds :: Core.Maybe Core.Natural,
    -- | Metadata which can be used to manage the role alias.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoleAlias' value with any optional fields omitted.
mkCreateRoleAlias ::
  -- | 'roleAlias'
  Types.RoleAlias ->
  -- | 'roleArn'
  Types.RoleArn ->
  CreateRoleAlias
mkCreateRoleAlias roleAlias roleArn =
  CreateRoleAlias'
    { roleAlias,
      roleArn,
      credentialDurationSeconds = Core.Nothing,
      tags = Core.Nothing
    }

-- | The role alias that points to a role ARN. This allows you to change the role without having to update the device.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleAlias :: Lens.Lens' CreateRoleAlias Types.RoleAlias
craRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED craRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleArn :: Lens.Lens' CreateRoleAlias Types.RoleArn
craRoleArn = Lens.field @"roleArn"
{-# DEPRECATED craRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | How long (in seconds) the credentials will be valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craCredentialDurationSeconds :: Lens.Lens' CreateRoleAlias (Core.Maybe Core.Natural)
craCredentialDurationSeconds = Lens.field @"credentialDurationSeconds"
{-# DEPRECATED craCredentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead." #-}

-- | Metadata which can be used to manage the role alias.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craTags :: Lens.Lens' CreateRoleAlias (Core.Maybe [Types.Tag])
craTags = Lens.field @"tags"
{-# DEPRECATED craTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRoleAlias where
  toJSON CreateRoleAlias {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            ("credentialDurationSeconds" Core..=)
              Core.<$> credentialDurationSeconds,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRoleAlias where
  type Rs CreateRoleAlias = CreateRoleAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/role-aliases/" Core.<> (Core.toText roleAlias)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoleAliasResponse'
            Core.<$> (x Core..:? "roleAlias")
            Core.<*> (x Core..:? "roleAliasArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { -- | The role alias.
    roleAlias :: Core.Maybe Types.RoleAlias,
    -- | The role alias ARN.
    roleAliasArn :: Core.Maybe Types.RoleAliasArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoleAliasResponse' value with any optional fields omitted.
mkCreateRoleAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRoleAliasResponse
mkCreateRoleAliasResponse responseStatus =
  CreateRoleAliasResponse'
    { roleAlias = Core.Nothing,
      roleAliasArn = Core.Nothing,
      responseStatus
    }

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsRoleAlias :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Types.RoleAlias)
crarrsRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED crarrsRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The role alias ARN.
--
-- /Note:/ Consider using 'roleAliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsRoleAliasArn :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Types.RoleAliasArn)
crarrsRoleAliasArn = Lens.field @"roleAliasArn"
{-# DEPRECATED crarrsRoleAliasArn "Use generic-lens or generic-optics with 'roleAliasArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsResponseStatus :: Lens.Lens' CreateRoleAliasResponse Core.Int
crarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

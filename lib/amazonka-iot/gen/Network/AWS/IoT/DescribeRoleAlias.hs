{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a role alias.
module Network.AWS.IoT.DescribeRoleAlias
  ( -- * Creating a request
    DescribeRoleAlias (..),
    mkDescribeRoleAlias,

    -- ** Request lenses
    draRoleAlias,

    -- * Destructuring the response
    DescribeRoleAliasResponse (..),
    mkDescribeRoleAliasResponse,

    -- ** Response lenses
    drarrsRoleAliasDescription,
    drarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRoleAlias' smart constructor.
newtype DescribeRoleAlias = DescribeRoleAlias'
  { -- | The role alias to describe.
    roleAlias :: Types.RoleAlias
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRoleAlias' value with any optional fields omitted.
mkDescribeRoleAlias ::
  -- | 'roleAlias'
  Types.RoleAlias ->
  DescribeRoleAlias
mkDescribeRoleAlias roleAlias = DescribeRoleAlias' {roleAlias}

-- | The role alias to describe.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRoleAlias :: Lens.Lens' DescribeRoleAlias Types.RoleAlias
draRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED draRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

instance Core.AWSRequest DescribeRoleAlias where
  type Rs DescribeRoleAlias = DescribeRoleAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/role-aliases/" Core.<> (Core.toText roleAlias)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRoleAliasResponse'
            Core.<$> (x Core..:? "roleAliasDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeRoleAliasResponse' smart constructor.
data DescribeRoleAliasResponse = DescribeRoleAliasResponse'
  { -- | The role alias description.
    roleAliasDescription :: Core.Maybe Types.RoleAliasDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeRoleAliasResponse' value with any optional fields omitted.
mkDescribeRoleAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRoleAliasResponse
mkDescribeRoleAliasResponse responseStatus =
  DescribeRoleAliasResponse'
    { roleAliasDescription = Core.Nothing,
      responseStatus
    }

-- | The role alias description.
--
-- /Note:/ Consider using 'roleAliasDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsRoleAliasDescription :: Lens.Lens' DescribeRoleAliasResponse (Core.Maybe Types.RoleAliasDescription)
drarrsRoleAliasDescription = Lens.field @"roleAliasDescription"
{-# DEPRECATED drarrsRoleAliasDescription "Use generic-lens or generic-optics with 'roleAliasDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsResponseStatus :: Lens.Lens' DescribeRoleAliasResponse Core.Int
drarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

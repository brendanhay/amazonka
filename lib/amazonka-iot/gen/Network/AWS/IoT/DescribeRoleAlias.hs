{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeRoleAlias (..)
    , mkDescribeRoleAlias
    -- ** Request lenses
    , draRoleAlias

    -- * Destructuring the response
    , DescribeRoleAliasResponse (..)
    , mkDescribeRoleAliasResponse
    -- ** Response lenses
    , drarrsRoleAliasDescription
    , drarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRoleAlias' smart constructor.
newtype DescribeRoleAlias = DescribeRoleAlias'
  { roleAlias :: Types.RoleAlias
    -- ^ The role alias to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRoleAlias' value with any optional fields omitted.
mkDescribeRoleAlias
    :: Types.RoleAlias -- ^ 'roleAlias'
    -> DescribeRoleAlias
mkDescribeRoleAlias roleAlias = DescribeRoleAlias'{roleAlias}

-- | The role alias to describe.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRoleAlias :: Lens.Lens' DescribeRoleAlias Types.RoleAlias
draRoleAlias = Lens.field @"roleAlias"
{-# INLINEABLE draRoleAlias #-}
{-# DEPRECATED roleAlias "Use generic-lens or generic-optics with 'roleAlias' instead"  #-}

instance Core.ToQuery DescribeRoleAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRoleAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeRoleAlias where
        type Rs DescribeRoleAlias = DescribeRoleAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/role-aliases/" Core.<> Core.toText roleAlias,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRoleAliasResponse' Core.<$>
                   (x Core..:? "roleAliasDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRoleAliasResponse' smart constructor.
data DescribeRoleAliasResponse = DescribeRoleAliasResponse'
  { roleAliasDescription :: Core.Maybe Types.RoleAliasDescription
    -- ^ The role alias description.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRoleAliasResponse' value with any optional fields omitted.
mkDescribeRoleAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRoleAliasResponse
mkDescribeRoleAliasResponse responseStatus
  = DescribeRoleAliasResponse'{roleAliasDescription = Core.Nothing,
                               responseStatus}

-- | The role alias description.
--
-- /Note:/ Consider using 'roleAliasDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsRoleAliasDescription :: Lens.Lens' DescribeRoleAliasResponse (Core.Maybe Types.RoleAliasDescription)
drarrsRoleAliasDescription = Lens.field @"roleAliasDescription"
{-# INLINEABLE drarrsRoleAliasDescription #-}
{-# DEPRECATED roleAliasDescription "Use generic-lens or generic-optics with 'roleAliasDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsResponseStatus :: Lens.Lens' DescribeRoleAliasResponse Core.Int
drarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

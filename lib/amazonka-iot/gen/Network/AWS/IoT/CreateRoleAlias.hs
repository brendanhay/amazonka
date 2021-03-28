{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRoleAlias (..)
    , mkCreateRoleAlias
    -- ** Request lenses
    , craRoleAlias
    , craRoleArn
    , craCredentialDurationSeconds
    , craTags

    -- * Destructuring the response
    , CreateRoleAliasResponse (..)
    , mkCreateRoleAliasResponse
    -- ** Response lenses
    , crarrsRoleAlias
    , crarrsRoleAliasArn
    , crarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { roleAlias :: Types.RoleAlias
    -- ^ The role alias that points to a role ARN. This allows you to change the role without having to update the device.
  , roleArn :: Types.RoleArn
    -- ^ The role ARN.
  , credentialDurationSeconds :: Core.Maybe Core.Natural
    -- ^ How long (in seconds) the credentials will be valid.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the role alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoleAlias' value with any optional fields omitted.
mkCreateRoleAlias
    :: Types.RoleAlias -- ^ 'roleAlias'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateRoleAlias
mkCreateRoleAlias roleAlias roleArn
  = CreateRoleAlias'{roleAlias, roleArn,
                     credentialDurationSeconds = Core.Nothing, tags = Core.Nothing}

-- | The role alias that points to a role ARN. This allows you to change the role without having to update the device.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleAlias :: Lens.Lens' CreateRoleAlias Types.RoleAlias
craRoleAlias = Lens.field @"roleAlias"
{-# INLINEABLE craRoleAlias #-}
{-# DEPRECATED roleAlias "Use generic-lens or generic-optics with 'roleAlias' instead"  #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleArn :: Lens.Lens' CreateRoleAlias Types.RoleArn
craRoleArn = Lens.field @"roleArn"
{-# INLINEABLE craRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | How long (in seconds) the credentials will be valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craCredentialDurationSeconds :: Lens.Lens' CreateRoleAlias (Core.Maybe Core.Natural)
craCredentialDurationSeconds = Lens.field @"credentialDurationSeconds"
{-# INLINEABLE craCredentialDurationSeconds #-}
{-# DEPRECATED credentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead"  #-}

-- | Metadata which can be used to manage the role alias.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craTags :: Lens.Lens' CreateRoleAlias (Core.Maybe [Types.Tag])
craTags = Lens.field @"tags"
{-# INLINEABLE craTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRoleAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRoleAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateRoleAlias where
        toJSON CreateRoleAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  ("credentialDurationSeconds" Core..=) Core.<$>
                    credentialDurationSeconds,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRoleAlias where
        type Rs CreateRoleAlias = CreateRoleAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/role-aliases/" Core.<> Core.toText roleAlias,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRoleAliasResponse' Core.<$>
                   (x Core..:? "roleAlias") Core.<*> x Core..:? "roleAliasArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { roleAlias :: Core.Maybe Types.RoleAlias
    -- ^ The role alias.
  , roleAliasArn :: Core.Maybe Types.RoleAliasArn
    -- ^ The role alias ARN.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoleAliasResponse' value with any optional fields omitted.
mkCreateRoleAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRoleAliasResponse
mkCreateRoleAliasResponse responseStatus
  = CreateRoleAliasResponse'{roleAlias = Core.Nothing,
                             roleAliasArn = Core.Nothing, responseStatus}

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsRoleAlias :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Types.RoleAlias)
crarrsRoleAlias = Lens.field @"roleAlias"
{-# INLINEABLE crarrsRoleAlias #-}
{-# DEPRECATED roleAlias "Use generic-lens or generic-optics with 'roleAlias' instead"  #-}

-- | The role alias ARN.
--
-- /Note:/ Consider using 'roleAliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsRoleAliasArn :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Types.RoleAliasArn)
crarrsRoleAliasArn = Lens.field @"roleAliasArn"
{-# INLINEABLE crarrsRoleAliasArn #-}
{-# DEPRECATED roleAliasArn "Use generic-lens or generic-optics with 'roleAliasArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarrsResponseStatus :: Lens.Lens' CreateRoleAliasResponse Core.Int
crarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

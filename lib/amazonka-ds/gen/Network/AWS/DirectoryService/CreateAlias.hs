{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as @http://<alias>.awsapps.com@ .
--
-- /Important:/ After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.
module Network.AWS.DirectoryService.CreateAlias
    (
    -- * Creating a request
      CreateAlias (..)
    , mkCreateAlias
    -- ** Request lenses
    , caDirectoryId
    , caAlias

    -- * Destructuring the response
    , CreateAliasResponse (..)
    , mkCreateAliasResponse
    -- ** Response lenses
    , carrsAlias
    , carrsDirectoryId
    , carrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory for which to create the alias.
  , alias :: Types.AliasName
    -- ^ The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlias' value with any optional fields omitted.
mkCreateAlias
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.AliasName -- ^ 'alias'
    -> CreateAlias
mkCreateAlias directoryId alias = CreateAlias'{directoryId, alias}

-- | The identifier of the directory for which to create the alias.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDirectoryId :: Lens.Lens' CreateAlias Types.DirectoryId
caDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE caDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlias :: Lens.Lens' CreateAlias Types.AliasName
caAlias = Lens.field @"alias"
{-# INLINEABLE caAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

instance Core.ToQuery CreateAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAlias where
        toHeaders CreateAlias{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateAlias")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAlias where
        toJSON CreateAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("Alias" Core..= alias)])

instance Core.AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAliasResponse' Core.<$>
                   (x Core..:? "Alias") Core.<*> x Core..:? "DirectoryId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { alias :: Core.Maybe Types.Alias
    -- ^ The alias for the directory.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAliasResponse' value with any optional fields omitted.
mkCreateAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAliasResponse
mkCreateAliasResponse responseStatus
  = CreateAliasResponse'{alias = Core.Nothing,
                         directoryId = Core.Nothing, responseStatus}

-- | The alias for the directory.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAlias :: Lens.Lens' CreateAliasResponse (Core.Maybe Types.Alias)
carrsAlias = Lens.field @"alias"
{-# INLINEABLE carrsAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsDirectoryId :: Lens.Lens' CreateAliasResponse (Core.Maybe Types.DirectoryId)
carrsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE carrsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAliasResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

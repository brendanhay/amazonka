{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caDirectoryId,
    caAlias,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,

    -- ** Response lenses
    carrsAlias,
    carrsDirectoryId,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The identifier of the directory for which to create the alias.
    directoryId :: Types.DirectoryId,
    -- | The requested alias.
    --
    -- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
    alias :: Types.AliasName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlias' value with any optional fields omitted.
mkCreateAlias ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'alias'
  Types.AliasName ->
  CreateAlias
mkCreateAlias directoryId alias = CreateAlias' {directoryId, alias}

-- | The identifier of the directory for which to create the alias.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDirectoryId :: Lens.Lens' CreateAlias Types.DirectoryId
caDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED caDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlias :: Lens.Lens' CreateAlias Types.AliasName
caAlias = Lens.field @"alias"
{-# DEPRECATED caAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

instance Core.FromJSON CreateAlias where
  toJSON CreateAlias {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Alias" Core..= alias)
          ]
      )

instance Core.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DirectoryService_20150416.CreateAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Core.<$> (x Core..:? "Alias")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The alias for the directory.
    alias :: Core.Maybe Types.Alias,
    -- | The identifier of the directory.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAliasResponse' value with any optional fields omitted.
mkCreateAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAliasResponse
mkCreateAliasResponse responseStatus =
  CreateAliasResponse'
    { alias = Core.Nothing,
      directoryId = Core.Nothing,
      responseStatus
    }

-- | The alias for the directory.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAlias :: Lens.Lens' CreateAliasResponse (Core.Maybe Types.Alias)
carrsAlias = Lens.field @"alias"
{-# DEPRECATED carrsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsDirectoryId :: Lens.Lens' CreateAliasResponse (Core.Maybe Types.DirectoryId)
carrsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED carrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAliasResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

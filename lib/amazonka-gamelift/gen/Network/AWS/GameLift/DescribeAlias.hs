{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for an alias. This operation returns all alias metadata and settings. To get an alias's target fleet ID only, use @ResolveAlias@ .
--
-- To get alias properties, specify the alias ID. If successful, the requested alias record is returned.
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.DescribeAlias
  ( -- * Creating a request
    DescribeAlias (..),
    mkDescribeAlias,

    -- ** Request lenses
    dAliasId,

    -- * Destructuring the response
    DescribeAliasResponse (..),
    mkDescribeAliasResponse,

    -- ** Response lenses
    darrsAlias,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeAlias' smart constructor.
newtype DescribeAlias = DescribeAlias'
  { -- | The unique identifier for the fleet alias that you want to retrieve. You can use either the alias ID or ARN value.
    aliasId :: Types.AliasId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAlias' value with any optional fields omitted.
mkDescribeAlias ::
  -- | 'aliasId'
  Types.AliasId ->
  DescribeAlias
mkDescribeAlias aliasId = DescribeAlias' {aliasId}

-- | The unique identifier for the fleet alias that you want to retrieve. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasId :: Lens.Lens' DescribeAlias Types.AliasId
dAliasId = Lens.field @"aliasId"
{-# DEPRECATED dAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Core.FromJSON DescribeAlias where
  toJSON DescribeAlias {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest DescribeAlias where
  type Rs DescribeAlias = DescribeAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAliasResponse'
            Core.<$> (x Core..:? "Alias") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeAliasResponse' smart constructor.
data DescribeAliasResponse = DescribeAliasResponse'
  { -- | The requested alias resource.
    alias :: Core.Maybe Types.Alias,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAliasResponse' value with any optional fields omitted.
mkDescribeAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAliasResponse
mkDescribeAliasResponse responseStatus =
  DescribeAliasResponse' {alias = Core.Nothing, responseStatus}

-- | The requested alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAlias :: Lens.Lens' DescribeAliasResponse (Core.Maybe Types.Alias)
darrsAlias = Lens.field @"alias"
{-# DEPRECATED darrsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAliasResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

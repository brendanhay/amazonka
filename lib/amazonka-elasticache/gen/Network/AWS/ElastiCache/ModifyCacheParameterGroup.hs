{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
  ( -- * Creating a request
    ModifyCacheParameterGroup (..),
    mkModifyCacheParameterGroup,

    -- ** Request lenses
    mcpgCacheParameterGroupName,
    mcpgParameterNameValues,

    -- * Destructuring the response
    Types.CacheParameterGroupNameMessage (..),
    Types.mkCacheParameterGroupNameMessage,

    -- ** Response lenses
    Types.cpgnmCacheParameterGroupName,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheParameterGroup@ operation.
--
-- /See:/ 'mkModifyCacheParameterGroup' smart constructor.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'
  { -- | The name of the cache parameter group to modify.
    cacheParameterGroupName :: Types.String,
    -- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
    parameterNameValues :: [Types.ParameterNameValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheParameterGroup' value with any optional fields omitted.
mkModifyCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Types.String ->
  ModifyCacheParameterGroup
mkModifyCacheParameterGroup cacheParameterGroupName =
  ModifyCacheParameterGroup'
    { cacheParameterGroupName,
      parameterNameValues = Core.mempty
    }

-- | The name of the cache parameter group to modify.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgCacheParameterGroupName :: Lens.Lens' ModifyCacheParameterGroup Types.String
mcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED mcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameterNameValues :: Lens.Lens' ModifyCacheParameterGroup [Types.ParameterNameValue]
mcpgParameterNameValues = Lens.field @"parameterNameValues"
{-# DEPRECATED mcpgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

instance Core.AWSRequest ModifyCacheParameterGroup where
  type
    Rs ModifyCacheParameterGroup =
      Types.CacheParameterGroupNameMessage
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyCacheParameterGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "CacheParameterGroupName"
                            cacheParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "ParameterNameValues"
                            (Core.toQueryList "ParameterNameValue" parameterNameValues)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyCacheParameterGroupResult"
      (\s h x -> Core.parseXML x)

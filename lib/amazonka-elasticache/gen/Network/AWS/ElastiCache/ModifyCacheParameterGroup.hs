{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyCacheParameterGroup (..)
    , mkModifyCacheParameterGroup
    -- ** Request lenses
    , mcpgCacheParameterGroupName
    , mcpgParameterNameValues

     -- * Destructuring the response
    , Types.CacheParameterGroupNameMessage (..)
    , Types.mkCacheParameterGroupNameMessage
    -- ** Response lenses
    , Types.cpgnmCacheParameterGroupName
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheParameterGroup@ operation.
--
-- /See:/ 'mkModifyCacheParameterGroup' smart constructor.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'
  { cacheParameterGroupName :: Core.Text
    -- ^ The name of the cache parameter group to modify.
  , parameterNameValues :: [Types.ParameterNameValue]
    -- ^ An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheParameterGroup' value with any optional fields omitted.
mkModifyCacheParameterGroup
    :: Core.Text -- ^ 'cacheParameterGroupName'
    -> ModifyCacheParameterGroup
mkModifyCacheParameterGroup cacheParameterGroupName
  = ModifyCacheParameterGroup'{cacheParameterGroupName,
                               parameterNameValues = Core.mempty}

-- | The name of the cache parameter group to modify.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgCacheParameterGroupName :: Lens.Lens' ModifyCacheParameterGroup Core.Text
mcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE mcpgCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameterNameValues :: Lens.Lens' ModifyCacheParameterGroup [Types.ParameterNameValue]
mcpgParameterNameValues = Lens.field @"parameterNameValues"
{-# INLINEABLE mcpgParameterNameValues #-}
{-# DEPRECATED parameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead"  #-}

instance Core.ToQuery ModifyCacheParameterGroup where
        toQuery ModifyCacheParameterGroup{..}
          = Core.toQueryPair "Action"
              ("ModifyCacheParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheParameterGroupName" cacheParameterGroupName
              Core.<>
              Core.toQueryPair "ParameterNameValues"
                (Core.toQueryList "ParameterNameValue" parameterNameValues)

instance Core.ToHeaders ModifyCacheParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCacheParameterGroup where
        type Rs ModifyCacheParameterGroup =
             Types.CacheParameterGroupNameMessage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyCacheParameterGroupResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the @ResetAllParameters@ and @CacheParameterGroupName@ parameters.
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Creating a request
      ResetCacheParameterGroup (..)
    , mkResetCacheParameterGroup
    -- ** Request lenses
    , rcpgCacheParameterGroupName
    , rcpgParameterNameValues
    , rcpgResetAllParameters

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

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
-- /See:/ 'mkResetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { cacheParameterGroupName :: Core.Text
    -- ^ The name of the cache parameter group to reset.
  , parameterNameValues :: Core.Maybe [Types.ParameterNameValue]
    -- ^ An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
  , resetAllParameters :: Core.Maybe Core.Bool
    -- ^ If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetCacheParameterGroup' value with any optional fields omitted.
mkResetCacheParameterGroup
    :: Core.Text -- ^ 'cacheParameterGroupName'
    -> ResetCacheParameterGroup
mkResetCacheParameterGroup cacheParameterGroupName
  = ResetCacheParameterGroup'{cacheParameterGroupName,
                              parameterNameValues = Core.Nothing,
                              resetAllParameters = Core.Nothing}

-- | The name of the cache parameter group to reset.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgCacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Core.Text
rcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE rcpgCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe [Types.ParameterNameValue])
rcpgParameterNameValues = Lens.field @"parameterNameValues"
{-# INLINEABLE rcpgParameterNameValues #-}
{-# DEPRECATED parameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead"  #-}

-- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@ 
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe Core.Bool)
rcpgResetAllParameters = Lens.field @"resetAllParameters"
{-# INLINEABLE rcpgResetAllParameters #-}
{-# DEPRECATED resetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead"  #-}

instance Core.ToQuery ResetCacheParameterGroup where
        toQuery ResetCacheParameterGroup{..}
          = Core.toQueryPair "Action"
              ("ResetCacheParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheParameterGroupName" cacheParameterGroupName
              Core.<>
              Core.toQueryPair "ParameterNameValues"
                (Core.maybe Core.mempty (Core.toQueryList "ParameterNameValue")
                   parameterNameValues)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResetAllParameters")
                resetAllParameters

instance Core.ToHeaders ResetCacheParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetCacheParameterGroup where
        type Rs ResetCacheParameterGroup =
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
          = Response.receiveXMLWrapper "ResetCacheParameterGroupResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

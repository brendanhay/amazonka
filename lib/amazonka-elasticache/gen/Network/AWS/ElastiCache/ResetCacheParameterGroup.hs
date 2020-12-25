{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ResetCacheParameterGroup (..),
    mkResetCacheParameterGroup,

    -- ** Request lenses
    rcpgCacheParameterGroupName,
    rcpgParameterNameValues,
    rcpgResetAllParameters,

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

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
-- /See:/ 'mkResetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { -- | The name of the cache parameter group to reset.
    cacheParameterGroupName :: Types.String,
    -- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
    parameterNameValues :: Core.Maybe [Types.ParameterNameValue],
    -- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
    --
    -- Valid values: @true@ | @false@
    resetAllParameters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetCacheParameterGroup' value with any optional fields omitted.
mkResetCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Types.String ->
  ResetCacheParameterGroup
mkResetCacheParameterGroup cacheParameterGroupName =
  ResetCacheParameterGroup'
    { cacheParameterGroupName,
      parameterNameValues = Core.Nothing,
      resetAllParameters = Core.Nothing
    }

-- | The name of the cache parameter group to reset.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgCacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Types.String
rcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED rcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe [Types.ParameterNameValue])
rcpgParameterNameValues = Lens.field @"parameterNameValues"
{-# DEPRECATED rcpgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

-- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe Core.Bool)
rcpgResetAllParameters = Lens.field @"resetAllParameters"
{-# DEPRECATED rcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

instance Core.AWSRequest ResetCacheParameterGroup where
  type
    Rs ResetCacheParameterGroup =
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
            ( Core.pure ("Action", "ResetCacheParameterGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "CacheParameterGroupName"
                            cacheParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "ParameterNameValues"
                            ( Core.toQueryList "ParameterNameValue"
                                Core.<$> parameterNameValues
                            )
                        )
                Core.<> ( Core.toQueryValue "ResetAllParameters"
                            Core.<$> resetAllParameters
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ResetCacheParameterGroupResult"
      (\s h x -> Core.parseXML x)

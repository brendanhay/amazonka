{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.EngineDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.EngineDefaults
  ( EngineDefaults (..),

    -- * Smart constructor
    mkEngineDefaults,

    -- * Lenses
    edCacheNodeTypeSpecificParameters,
    edCacheParameterGroupFamily,
    edMarker,
    edParameters,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter as Types
import qualified Network.AWS.ElastiCache.Types.Parameter as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @DescribeEngineDefaultParameters@ operation.
--
-- /See:/ 'mkEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { -- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Core.Maybe [Types.CacheNodeTypeSpecificParameter],
    -- | Specifies the name of the cache parameter group family to which the engine default parameters apply.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
    cacheParameterGroupFamily :: Core.Maybe Types.String,
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.String,
    -- | Contains a list of engine default parameters.
    parameters :: Core.Maybe [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EngineDefaults' value with any optional fields omitted.
mkEngineDefaults ::
  EngineDefaults
mkEngineDefaults =
  EngineDefaults'
    { cacheNodeTypeSpecificParameters = Core.Nothing,
      cacheParameterGroupFamily = Core.Nothing,
      marker = Core.Nothing,
      parameters = Core.Nothing
    }

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCacheNodeTypeSpecificParameters :: Lens.Lens' EngineDefaults (Core.Maybe [Types.CacheNodeTypeSpecificParameter])
edCacheNodeTypeSpecificParameters = Lens.field @"cacheNodeTypeSpecificParameters"
{-# DEPRECATED edCacheNodeTypeSpecificParameters "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificParameters' instead." #-}

-- | Specifies the name of the cache parameter group family to which the engine default parameters apply.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCacheParameterGroupFamily :: Lens.Lens' EngineDefaults (Core.Maybe Types.String)
edCacheParameterGroupFamily = Lens.field @"cacheParameterGroupFamily"
{-# DEPRECATED edCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMarker :: Lens.Lens' EngineDefaults (Core.Maybe Types.String)
edMarker = Lens.field @"marker"
{-# DEPRECATED edMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains a list of engine default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edParameters :: Lens.Lens' EngineDefaults (Core.Maybe [Types.Parameter])
edParameters = Lens.field @"parameters"
{-# DEPRECATED edParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      Core.<$> ( x Core..@? "CacheNodeTypeSpecificParameters"
                   Core..<@> Core.parseXMLList "CacheNodeTypeSpecificParameter"
               )
      Core.<*> (x Core..@? "CacheParameterGroupFamily")
      Core.<*> (x Core..@? "Marker")
      Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")

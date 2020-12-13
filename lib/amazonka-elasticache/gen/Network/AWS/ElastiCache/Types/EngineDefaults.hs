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
    edCacheParameterGroupFamily,
    edCacheNodeTypeSpecificParameters,
    edMarker,
    edParameters,
  )
where

import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Network.AWS.ElastiCache.Types.Parameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @DescribeEngineDefaultParameters@ operation.
--
-- /See:/ 'mkEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { -- | Specifies the name of the cache parameter group family to which the engine default parameters apply.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
    cacheParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Lude.Maybe [CacheNodeTypeSpecificParameter],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Lude.Maybe Lude.Text,
    -- | Contains a list of engine default parameters.
    parameters :: Lude.Maybe [Parameter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupFamily' - Specifies the name of the cache parameter group family to which the engine default parameters apply.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
-- * 'cacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'parameters' - Contains a list of engine default parameters.
mkEngineDefaults ::
  EngineDefaults
mkEngineDefaults =
  EngineDefaults'
    { cacheParameterGroupFamily = Lude.Nothing,
      cacheNodeTypeSpecificParameters = Lude.Nothing,
      marker = Lude.Nothing,
      parameters = Lude.Nothing
    }

-- | Specifies the name of the cache parameter group family to which the engine default parameters apply.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCacheParameterGroupFamily :: Lens.Lens' EngineDefaults (Lude.Maybe Lude.Text)
edCacheParameterGroupFamily = Lens.lens (cacheParameterGroupFamily :: EngineDefaults -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupFamily = a} :: EngineDefaults)
{-# DEPRECATED edCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCacheNodeTypeSpecificParameters :: Lens.Lens' EngineDefaults (Lude.Maybe [CacheNodeTypeSpecificParameter])
edCacheNodeTypeSpecificParameters = Lens.lens (cacheNodeTypeSpecificParameters :: EngineDefaults -> Lude.Maybe [CacheNodeTypeSpecificParameter]) (\s a -> s {cacheNodeTypeSpecificParameters = a} :: EngineDefaults)
{-# DEPRECATED edCacheNodeTypeSpecificParameters "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificParameters' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMarker :: Lens.Lens' EngineDefaults (Lude.Maybe Lude.Text)
edMarker = Lens.lens (marker :: EngineDefaults -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: EngineDefaults)
{-# DEPRECATED edMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains a list of engine default parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edParameters :: Lens.Lens' EngineDefaults (Lude.Maybe [Parameter])
edParameters = Lens.lens (parameters :: EngineDefaults -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: EngineDefaults)
{-# DEPRECATED edParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      Lude.<$> (x Lude..@? "CacheParameterGroupFamily")
      Lude.<*> ( x Lude..@? "CacheNodeTypeSpecificParameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeTypeSpecificParameter")
               )
      Lude.<*> (x Lude..@? "Marker")
      Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
               )

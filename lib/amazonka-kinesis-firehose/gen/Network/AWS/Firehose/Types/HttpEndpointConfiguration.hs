{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointConfiguration
  ( HttpEndpointConfiguration (..)
  -- * Smart constructor
  , mkHttpEndpointConfiguration
  -- * Lenses
  , hecUrl
  , hecAccessKey
  , hecName
  ) where

import qualified Network.AWS.Firehose.Types.HttpEndpointAccessKey as Types
import qualified Network.AWS.Firehose.Types.Name as Types
import qualified Network.AWS.Firehose.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of the HTTP endpoint to which Kinesis Firehose delivers data.
--
-- /See:/ 'mkHttpEndpointConfiguration' smart constructor.
data HttpEndpointConfiguration = HttpEndpointConfiguration'
  { url :: Types.Url
    -- ^ The URL of the HTTP endpoint selected as the destination.
  , accessKey :: Core.Maybe Types.HttpEndpointAccessKey
    -- ^ The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the HTTP endpoint selected as the destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointConfiguration' value with any optional fields omitted.
mkHttpEndpointConfiguration
    :: Types.Url -- ^ 'url'
    -> HttpEndpointConfiguration
mkHttpEndpointConfiguration url
  = HttpEndpointConfiguration'{url, accessKey = Core.Nothing,
                               name = Core.Nothing}

-- | The URL of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hecUrl :: Lens.Lens' HttpEndpointConfiguration Types.Url
hecUrl = Lens.field @"url"
{-# INLINEABLE hecUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'accessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hecAccessKey :: Lens.Lens' HttpEndpointConfiguration (Core.Maybe Types.HttpEndpointAccessKey)
hecAccessKey = Lens.field @"accessKey"
{-# INLINEABLE hecAccessKey #-}
{-# DEPRECATED accessKey "Use generic-lens or generic-optics with 'accessKey' instead"  #-}

-- | The name of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hecName :: Lens.Lens' HttpEndpointConfiguration (Core.Maybe Types.Name)
hecName = Lens.field @"name"
{-# INLINEABLE hecName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON HttpEndpointConfiguration where
        toJSON HttpEndpointConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Url" Core..= url),
                  ("AccessKey" Core..=) Core.<$> accessKey,
                  ("Name" Core..=) Core.<$> name])

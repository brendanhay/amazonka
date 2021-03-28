{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.BasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.BasePathMapping
  ( BasePathMapping (..)
  -- * Smart constructor
  , mkBasePathMapping
  -- * Lenses
  , bpmBasePath
  , bpmRestApiId
  , bpmStage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the base path that callers of the API must provide as part of the URL after the domain name.
--
-- A custom domain name plus a @BasePathMapping@ specification identifies a deployed 'RestApi' in a given stage of the owner 'Account' .<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names> 
--
-- /See:/ 'mkBasePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
  { basePath :: Core.Maybe Core.Text
    -- ^ The base path name that callers of the API must provide as part of the URL after the domain name.
  , restApiId :: Core.Maybe Core.Text
    -- ^ The string identifier of the associated 'RestApi' .
  , stage :: Core.Maybe Core.Text
    -- ^ The name of the associated stage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BasePathMapping' value with any optional fields omitted.
mkBasePathMapping
    :: BasePathMapping
mkBasePathMapping
  = BasePathMapping'{basePath = Core.Nothing,
                     restApiId = Core.Nothing, stage = Core.Nothing}

-- | The base path name that callers of the API must provide as part of the URL after the domain name.
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmBasePath :: Lens.Lens' BasePathMapping (Core.Maybe Core.Text)
bpmBasePath = Lens.field @"basePath"
{-# INLINEABLE bpmBasePath #-}
{-# DEPRECATED basePath "Use generic-lens or generic-optics with 'basePath' instead"  #-}

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmRestApiId :: Lens.Lens' BasePathMapping (Core.Maybe Core.Text)
bpmRestApiId = Lens.field @"restApiId"
{-# INLINEABLE bpmRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The name of the associated stage.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmStage :: Lens.Lens' BasePathMapping (Core.Maybe Core.Text)
bpmStage = Lens.field @"stage"
{-# INLINEABLE bpmStage #-}
{-# DEPRECATED stage "Use generic-lens or generic-optics with 'stage' instead"  #-}

instance Core.FromJSON BasePathMapping where
        parseJSON
          = Core.withObject "BasePathMapping" Core.$
              \ x ->
                BasePathMapping' Core.<$>
                  (x Core..:? "basePath") Core.<*> x Core..:? "restApiId" Core.<*>
                    x Core..:? "stage"

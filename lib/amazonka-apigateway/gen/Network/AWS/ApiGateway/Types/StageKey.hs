{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.StageKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.StageKey
  ( StageKey (..)
  -- * Smart constructor
  , mkStageKey
  -- * Lenses
  , skRestApiId
  , skStageName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A reference to a unique stage identified in the format @{restApiId}/{stage}@ .
--
-- /See:/ 'mkStageKey' smart constructor.
data StageKey = StageKey'
  { restApiId :: Core.Maybe Core.Text
    -- ^ The string identifier of the associated 'RestApi' .
  , stageName :: Core.Maybe Core.Text
    -- ^ The stage name associated with the stage key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StageKey' value with any optional fields omitted.
mkStageKey
    :: StageKey
mkStageKey
  = StageKey'{restApiId = Core.Nothing, stageName = Core.Nothing}

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skRestApiId :: Lens.Lens' StageKey (Core.Maybe Core.Text)
skRestApiId = Lens.field @"restApiId"
{-# INLINEABLE skRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The stage name associated with the stage key.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skStageName :: Lens.Lens' StageKey (Core.Maybe Core.Text)
skStageName = Lens.field @"stageName"
{-# INLINEABLE skStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.FromJSON StageKey where
        toJSON StageKey{..}
          = Core.object
              (Core.catMaybes
                 [("restApiId" Core..=) Core.<$> restApiId,
                  ("stageName" Core..=) Core.<$> stageName])

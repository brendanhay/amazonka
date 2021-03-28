{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerEngineType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerEngineType
  ( BrokerEngineType (..)
  -- * Smart constructor
  , mkBrokerEngineType
  -- * Lenses
  , betEngineType
  , betEngineVersions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.EngineType as Types
import qualified Network.AWS.MQ.Types.EngineVersion as Types
import qualified Network.AWS.Prelude as Core

-- | Types of broker engines.
--
-- /See:/ 'mkBrokerEngineType' smart constructor.
data BrokerEngineType = BrokerEngineType'
  { engineType :: Core.Maybe Types.EngineType
    -- ^ The type of broker engine.
  , engineVersions :: Core.Maybe [Types.EngineVersion]
    -- ^ The list of engine versions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BrokerEngineType' value with any optional fields omitted.
mkBrokerEngineType
    :: BrokerEngineType
mkBrokerEngineType
  = BrokerEngineType'{engineType = Core.Nothing,
                      engineVersions = Core.Nothing}

-- | The type of broker engine.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
betEngineType :: Lens.Lens' BrokerEngineType (Core.Maybe Types.EngineType)
betEngineType = Lens.field @"engineType"
{-# INLINEABLE betEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | The list of engine versions.
--
-- /Note:/ Consider using 'engineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
betEngineVersions :: Lens.Lens' BrokerEngineType (Core.Maybe [Types.EngineVersion])
betEngineVersions = Lens.field @"engineVersions"
{-# INLINEABLE betEngineVersions #-}
{-# DEPRECATED engineVersions "Use generic-lens or generic-optics with 'engineVersions' instead"  #-}

instance Core.FromJSON BrokerEngineType where
        parseJSON
          = Core.withObject "BrokerEngineType" Core.$
              \ x ->
                BrokerEngineType' Core.<$>
                  (x Core..:? "engineType") Core.<*> x Core..:? "engineVersions"

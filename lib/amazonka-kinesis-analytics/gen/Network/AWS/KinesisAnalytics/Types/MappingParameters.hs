{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.MappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.MappingParameters
  ( MappingParameters (..)
  -- * Smart constructor
  , mkMappingParameters
  -- * Lenses
  , mpCSVMappingParameters
  , mpJSONMappingParameters
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.CSVMappingParameters as Types
import qualified Network.AWS.KinesisAnalytics.Types.JSONMappingParameters as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- /See:/ 'mkMappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { cSVMappingParameters :: Core.Maybe Types.CSVMappingParameters
    -- ^ Provides additional mapping information when the record format uses delimiters (for example, CSV).
  , jSONMappingParameters :: Core.Maybe Types.JSONMappingParameters
    -- ^ Provides additional mapping information when JSON is the record format on the streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MappingParameters' value with any optional fields omitted.
mkMappingParameters
    :: MappingParameters
mkMappingParameters
  = MappingParameters'{cSVMappingParameters = Core.Nothing,
                       jSONMappingParameters = Core.Nothing}

-- | Provides additional mapping information when the record format uses delimiters (for example, CSV).
--
-- /Note:/ Consider using 'cSVMappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpCSVMappingParameters :: Lens.Lens' MappingParameters (Core.Maybe Types.CSVMappingParameters)
mpCSVMappingParameters = Lens.field @"cSVMappingParameters"
{-# INLINEABLE mpCSVMappingParameters #-}
{-# DEPRECATED cSVMappingParameters "Use generic-lens or generic-optics with 'cSVMappingParameters' instead"  #-}

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
-- /Note:/ Consider using 'jSONMappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpJSONMappingParameters :: Lens.Lens' MappingParameters (Core.Maybe Types.JSONMappingParameters)
mpJSONMappingParameters = Lens.field @"jSONMappingParameters"
{-# INLINEABLE mpJSONMappingParameters #-}
{-# DEPRECATED jSONMappingParameters "Use generic-lens or generic-optics with 'jSONMappingParameters' instead"  #-}

instance Core.FromJSON MappingParameters where
        toJSON MappingParameters{..}
          = Core.object
              (Core.catMaybes
                 [("CSVMappingParameters" Core..=) Core.<$> cSVMappingParameters,
                  ("JSONMappingParameters" Core..=) Core.<$> jSONMappingParameters])

instance Core.FromJSON MappingParameters where
        parseJSON
          = Core.withObject "MappingParameters" Core.$
              \ x ->
                MappingParameters' Core.<$>
                  (x Core..:? "CSVMappingParameters") Core.<*>
                    x Core..:? "JSONMappingParameters"

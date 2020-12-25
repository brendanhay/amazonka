{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OutputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputSerialization
  ( OutputSerialization (..),

    -- * Smart constructor
    mkOutputSerialization,

    -- * Lenses
    osCSV,
    osJSON,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.CSVOutput as Types
import qualified Network.AWS.S3.Types.JSONOutput as Types

-- | Describes how results of the Select job are serialized.
--
-- /See:/ 'mkOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded Select results.
    csv :: Core.Maybe Types.CSVOutput,
    -- | Specifies JSON as request's output serialization format.
    json :: Core.Maybe Types.JSONOutput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputSerialization' value with any optional fields omitted.
mkOutputSerialization ::
  OutputSerialization
mkOutputSerialization =
  OutputSerialization' {csv = Core.Nothing, json = Core.Nothing}

-- | Describes the serialization of CSV-encoded Select results.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCSV :: Lens.Lens' OutputSerialization (Core.Maybe Types.CSVOutput)
osCSV = Lens.field @"csv"
{-# DEPRECATED osCSV "Use generic-lens or generic-optics with 'csv' instead." #-}

-- | Specifies JSON as request's output serialization format.
--
-- /Note:/ Consider using 'json' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osJSON :: Lens.Lens' OutputSerialization (Core.Maybe Types.JSONOutput)
osJSON = Lens.field @"json"
{-# DEPRECATED osJSON "Use generic-lens or generic-optics with 'json' instead." #-}

instance Core.ToXML OutputSerialization where
  toXML OutputSerialization {..} =
    Core.toXMLNode "CSV" Core.<$> csv
      Core.<> Core.toXMLNode "JSON" Core.<$> json

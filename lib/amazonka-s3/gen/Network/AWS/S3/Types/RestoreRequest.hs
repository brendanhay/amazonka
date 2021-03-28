{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RestoreRequest
  ( RestoreRequest (..)
  -- * Smart constructor
  , mkRestoreRequest
  -- * Lenses
  , rrDays
  , rrDescription
  , rrGlacierJobParameters
  , rrOutputLocation
  , rrSelectParameters
  , rrTier
  , rrType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Description as Types
import qualified Network.AWS.S3.Types.GlacierJobParameters as Types
import qualified Network.AWS.S3.Types.OutputLocation as Types
import qualified Network.AWS.S3.Types.RestoreRequestType as Types
import qualified Network.AWS.S3.Types.SelectParameters as Types
import qualified Network.AWS.S3.Types.Tier as Types

-- | Container for restore job parameters.
--
-- /See:/ 'mkRestoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { days :: Core.Maybe Core.Int
    -- ^ Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
--
-- The Days element is required for regular restores, and must not be provided for select requests.
  , description :: Core.Maybe Types.Description
    -- ^ The optional description for the job.
  , glacierJobParameters :: Core.Maybe Types.GlacierJobParameters
    -- ^ S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
  , outputLocation :: Core.Maybe Types.OutputLocation
    -- ^ Describes the location where the restore job's output is stored.
  , selectParameters :: Core.Maybe Types.SelectParameters
    -- ^ Describes the parameters for Select job types.
  , tier :: Core.Maybe Types.Tier
    -- ^ Retrieval tier at which the restore will be processed.
  , type' :: Core.Maybe Types.RestoreRequestType
    -- ^ Type of restore request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreRequest' value with any optional fields omitted.
mkRestoreRequest
    :: RestoreRequest
mkRestoreRequest
  = RestoreRequest'{days = Core.Nothing, description = Core.Nothing,
                    glacierJobParameters = Core.Nothing, outputLocation = Core.Nothing,
                    selectParameters = Core.Nothing, tier = Core.Nothing,
                    type' = Core.Nothing}

-- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
--
-- The Days element is required for regular restores, and must not be provided for select requests.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDays :: Lens.Lens' RestoreRequest (Core.Maybe Core.Int)
rrDays = Lens.field @"days"
{-# INLINEABLE rrDays #-}
{-# DEPRECATED days "Use generic-lens or generic-optics with 'days' instead"  #-}

-- | The optional description for the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDescription :: Lens.Lens' RestoreRequest (Core.Maybe Types.Description)
rrDescription = Lens.field @"description"
{-# INLINEABLE rrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
--
-- /Note:/ Consider using 'glacierJobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGlacierJobParameters :: Lens.Lens' RestoreRequest (Core.Maybe Types.GlacierJobParameters)
rrGlacierJobParameters = Lens.field @"glacierJobParameters"
{-# INLINEABLE rrGlacierJobParameters #-}
{-# DEPRECATED glacierJobParameters "Use generic-lens or generic-optics with 'glacierJobParameters' instead"  #-}

-- | Describes the location where the restore job's output is stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrOutputLocation :: Lens.Lens' RestoreRequest (Core.Maybe Types.OutputLocation)
rrOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE rrOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | Describes the parameters for Select job types.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSelectParameters :: Lens.Lens' RestoreRequest (Core.Maybe Types.SelectParameters)
rrSelectParameters = Lens.field @"selectParameters"
{-# INLINEABLE rrSelectParameters #-}
{-# DEPRECATED selectParameters "Use generic-lens or generic-optics with 'selectParameters' instead"  #-}

-- | Retrieval tier at which the restore will be processed.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTier :: Lens.Lens' RestoreRequest (Core.Maybe Types.Tier)
rrTier = Lens.field @"tier"
{-# INLINEABLE rrTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | Type of restore request.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' RestoreRequest (Core.Maybe Types.RestoreRequestType)
rrType = Lens.field @"type'"
{-# INLINEABLE rrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToXML RestoreRequest where
        toXML RestoreRequest{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Days") days Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Description")
                description
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "GlacierJobParameters")
                glacierJobParameters
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "OutputLocation")
                outputLocation
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "SelectParameters")
                selectParameters
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Tier") tier
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Type") type'

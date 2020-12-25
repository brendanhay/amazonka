{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RestoreRequest
  ( RestoreRequest (..),

    -- * Smart constructor
    mkRestoreRequest,

    -- * Lenses
    rrDays,
    rrDescription,
    rrGlacierJobParameters,
    rrOutputLocation,
    rrSelectParameters,
    rrTier,
    rrType,
  )
where

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
  { -- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
    --
    -- The Days element is required for regular restores, and must not be provided for select requests.
    days :: Core.Maybe Core.Int,
    -- | The optional description for the job.
    description :: Core.Maybe Types.Description,
    -- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
    glacierJobParameters :: Core.Maybe Types.GlacierJobParameters,
    -- | Describes the location where the restore job's output is stored.
    outputLocation :: Core.Maybe Types.OutputLocation,
    -- | Describes the parameters for Select job types.
    selectParameters :: Core.Maybe Types.SelectParameters,
    -- | Retrieval tier at which the restore will be processed.
    tier :: Core.Maybe Types.Tier,
    -- | Type of restore request.
    type' :: Core.Maybe Types.RestoreRequestType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreRequest' value with any optional fields omitted.
mkRestoreRequest ::
  RestoreRequest
mkRestoreRequest =
  RestoreRequest'
    { days = Core.Nothing,
      description = Core.Nothing,
      glacierJobParameters = Core.Nothing,
      outputLocation = Core.Nothing,
      selectParameters = Core.Nothing,
      tier = Core.Nothing,
      type' = Core.Nothing
    }

-- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
--
-- The Days element is required for regular restores, and must not be provided for select requests.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDays :: Lens.Lens' RestoreRequest (Core.Maybe Core.Int)
rrDays = Lens.field @"days"
{-# DEPRECATED rrDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | The optional description for the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDescription :: Lens.Lens' RestoreRequest (Core.Maybe Types.Description)
rrDescription = Lens.field @"description"
{-# DEPRECATED rrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
--
-- /Note:/ Consider using 'glacierJobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGlacierJobParameters :: Lens.Lens' RestoreRequest (Core.Maybe Types.GlacierJobParameters)
rrGlacierJobParameters = Lens.field @"glacierJobParameters"
{-# DEPRECATED rrGlacierJobParameters "Use generic-lens or generic-optics with 'glacierJobParameters' instead." #-}

-- | Describes the location where the restore job's output is stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrOutputLocation :: Lens.Lens' RestoreRequest (Core.Maybe Types.OutputLocation)
rrOutputLocation = Lens.field @"outputLocation"
{-# DEPRECATED rrOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | Describes the parameters for Select job types.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSelectParameters :: Lens.Lens' RestoreRequest (Core.Maybe Types.SelectParameters)
rrSelectParameters = Lens.field @"selectParameters"
{-# DEPRECATED rrSelectParameters "Use generic-lens or generic-optics with 'selectParameters' instead." #-}

-- | Retrieval tier at which the restore will be processed.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTier :: Lens.Lens' RestoreRequest (Core.Maybe Types.Tier)
rrTier = Lens.field @"tier"
{-# DEPRECATED rrTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | Type of restore request.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' RestoreRequest (Core.Maybe Types.RestoreRequestType)
rrType = Lens.field @"type'"
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.ToXML RestoreRequest where
  toXML RestoreRequest {..} =
    Core.toXMLNode "Days" Core.<$> days
      Core.<> Core.toXMLNode "Description" Core.<$> description
      Core.<> Core.toXMLNode "GlacierJobParameters" Core.<$> glacierJobParameters
      Core.<> Core.toXMLNode "OutputLocation" Core.<$> outputLocation
      Core.<> Core.toXMLNode "SelectParameters" Core.<$> selectParameters
      Core.<> Core.toXMLNode "Tier" Core.<$> tier
      Core.<> Core.toXMLNode "Type" Core.<$> type'

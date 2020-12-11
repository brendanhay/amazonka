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
    rrSelectParameters,
    rrOutputLocation,
    rrTier,
    rrGlacierJobParameters,
    rrType,
    rrDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.OutputLocation
import Network.AWS.S3.Types.RestoreRequestType
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.Tier

-- | Container for restore job parameters.
--
-- /See:/ 'mkRestoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { days :: Lude.Maybe Lude.Int,
    selectParameters :: Lude.Maybe SelectParameters,
    outputLocation :: Lude.Maybe OutputLocation,
    tier :: Lude.Maybe Tier,
    glacierJobParameters :: Lude.Maybe GlacierJobParameters,
    type' :: Lude.Maybe RestoreRequestType,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreRequest' with the minimum fields required to make a request.
--
-- * 'days' - Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
--
-- The Days element is required for regular restores, and must not be provided for select requests.
-- * 'description' - The optional description for the job.
-- * 'glacierJobParameters' - S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
-- * 'outputLocation' - Describes the location where the restore job's output is stored.
-- * 'selectParameters' - Describes the parameters for Select job types.
-- * 'tier' - Retrieval tier at which the restore will be processed.
-- * 'type'' - Type of restore request.
mkRestoreRequest ::
  RestoreRequest
mkRestoreRequest =
  RestoreRequest'
    { days = Lude.Nothing,
      selectParameters = Lude.Nothing,
      outputLocation = Lude.Nothing,
      tier = Lude.Nothing,
      glacierJobParameters = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ .
--
-- The Days element is required for regular restores, and must not be provided for select requests.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDays :: Lens.Lens' RestoreRequest (Lude.Maybe Lude.Int)
rrDays = Lens.lens (days :: RestoreRequest -> Lude.Maybe Lude.Int) (\s a -> s {days = a} :: RestoreRequest)
{-# DEPRECATED rrDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | Describes the parameters for Select job types.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSelectParameters :: Lens.Lens' RestoreRequest (Lude.Maybe SelectParameters)
rrSelectParameters = Lens.lens (selectParameters :: RestoreRequest -> Lude.Maybe SelectParameters) (\s a -> s {selectParameters = a} :: RestoreRequest)
{-# DEPRECATED rrSelectParameters "Use generic-lens or generic-optics with 'selectParameters' instead." #-}

-- | Describes the location where the restore job's output is stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrOutputLocation :: Lens.Lens' RestoreRequest (Lude.Maybe OutputLocation)
rrOutputLocation = Lens.lens (outputLocation :: RestoreRequest -> Lude.Maybe OutputLocation) (\s a -> s {outputLocation = a} :: RestoreRequest)
{-# DEPRECATED rrOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | Retrieval tier at which the restore will be processed.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTier :: Lens.Lens' RestoreRequest (Lude.Maybe Tier)
rrTier = Lens.lens (tier :: RestoreRequest -> Lude.Maybe Tier) (\s a -> s {tier = a} :: RestoreRequest)
{-# DEPRECATED rrTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
--
-- /Note:/ Consider using 'glacierJobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGlacierJobParameters :: Lens.Lens' RestoreRequest (Lude.Maybe GlacierJobParameters)
rrGlacierJobParameters = Lens.lens (glacierJobParameters :: RestoreRequest -> Lude.Maybe GlacierJobParameters) (\s a -> s {glacierJobParameters = a} :: RestoreRequest)
{-# DEPRECATED rrGlacierJobParameters "Use generic-lens or generic-optics with 'glacierJobParameters' instead." #-}

-- | Type of restore request.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' RestoreRequest (Lude.Maybe RestoreRequestType)
rrType = Lens.lens (type' :: RestoreRequest -> Lude.Maybe RestoreRequestType) (\s a -> s {type' = a} :: RestoreRequest)
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The optional description for the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDescription :: Lens.Lens' RestoreRequest (Lude.Maybe Lude.Text)
rrDescription = Lens.lens (description :: RestoreRequest -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RestoreRequest)
{-# DEPRECATED rrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToXML RestoreRequest where
  toXML RestoreRequest' {..} =
    Lude.mconcat
      [ "Days" Lude.@= days,
        "SelectParameters" Lude.@= selectParameters,
        "OutputLocation" Lude.@= outputLocation,
        "Tier" Lude.@= tier,
        "GlacierJobParameters" Lude.@= glacierJobParameters,
        "Type" Lude.@= type',
        "Description" Lude.@= description
      ]

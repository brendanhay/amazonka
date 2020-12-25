{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputLocation
  ( InstanceAssociationOutputLocation (..),

    -- * Smart constructor
    mkInstanceAssociationOutputLocation,

    -- * Lenses
    iaolS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.S3OutputLocation as Types

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkInstanceAssociationOutputLocation' smart constructor.
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { -- | An S3 bucket where you want to store the results of this request.
    s3Location :: Core.Maybe Types.S3OutputLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceAssociationOutputLocation' value with any optional fields omitted.
mkInstanceAssociationOutputLocation ::
  InstanceAssociationOutputLocation
mkInstanceAssociationOutputLocation =
  InstanceAssociationOutputLocation' {s3Location = Core.Nothing}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaolS3Location :: Lens.Lens' InstanceAssociationOutputLocation (Core.Maybe Types.S3OutputLocation)
iaolS3Location = Lens.field @"s3Location"
{-# DEPRECATED iaolS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Core.FromJSON InstanceAssociationOutputLocation where
  toJSON InstanceAssociationOutputLocation {..} =
    Core.object
      (Core.catMaybes [("S3Location" Core..=) Core.<$> s3Location])

instance Core.FromJSON InstanceAssociationOutputLocation where
  parseJSON =
    Core.withObject "InstanceAssociationOutputLocation" Core.$
      \x ->
        InstanceAssociationOutputLocation'
          Core.<$> (x Core..:? "S3Location")

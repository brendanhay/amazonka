{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointFilter
  ( EndpointFilter (..),

    -- * Smart constructor
    mkEndpointFilter,

    -- * Lenses
    efCreationTimeAfter,
    efCreationTimeBefore,
    efModelArn,
    efStatus,
  )
where

import qualified Network.AWS.Comprehend.Types.ComprehendModelArn as Types
import qualified Network.AWS.Comprehend.Types.EndpointStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The filter used to determine which endpoints are returned. You can filter jobs on their name, model, status, or the date and time that they were created. You can only set one filter at a time.
--
-- /See:/ 'mkEndpointFilter' smart constructor.
data EndpointFilter = EndpointFilter'
  { -- | Specifies a date after which the returned endpoint or endpoints were created.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies a date before which the returned endpoint or endpoints were created.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
    modelArn :: Core.Maybe Types.ComprehendModelArn,
    -- | Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
    status :: Core.Maybe Types.EndpointStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EndpointFilter' value with any optional fields omitted.
mkEndpointFilter ::
  EndpointFilter
mkEndpointFilter =
  EndpointFilter'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      modelArn = Core.Nothing,
      status = Core.Nothing
    }

-- | Specifies a date after which the returned endpoint or endpoints were created.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCreationTimeAfter :: Lens.Lens' EndpointFilter (Core.Maybe Core.NominalDiffTime)
efCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED efCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | Specifies a date before which the returned endpoint or endpoints were created.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCreationTimeBefore :: Lens.Lens' EndpointFilter (Core.Maybe Core.NominalDiffTime)
efCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED efCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efModelArn :: Lens.Lens' EndpointFilter (Core.Maybe Types.ComprehendModelArn)
efModelArn = Lens.field @"modelArn"
{-# DEPRECATED efModelArn "Use generic-lens or generic-optics with 'modelArn' instead." #-}

-- | Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efStatus :: Lens.Lens' EndpointFilter (Core.Maybe Types.EndpointStatus)
efStatus = Lens.field @"status"
{-# DEPRECATED efStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON EndpointFilter where
  toJSON EndpointFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("ModelArn" Core..=) Core.<$> modelArn,
            ("Status" Core..=) Core.<$> status
          ]
      )

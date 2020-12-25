{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentLifecycle
  ( EnvironmentLifecycle (..),

    -- * Smart constructor
    mkEnvironmentLifecycle,

    -- * Lenses
    elFailureResource,
    elReason,
    elStatus,
  )
where

import qualified Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus as Types
import qualified Network.AWS.Cloud9.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the current creation or deletion lifecycle state of an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { -- | If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
    failureResource :: Core.Maybe Types.String,
    -- | Any informational message about the lifecycle state of the environment.
    reason :: Core.Maybe Types.String,
    -- | The current creation or deletion lifecycle state of the environment.
    --
    --
    --     * @CREATING@ : The environment is in the process of being created.
    --
    --
    --     * @CREATED@ : The environment was successfully created.
    --
    --
    --     * @CREATE_FAILED@ : The environment failed to be created.
    --
    --
    --     * @DELETING@ : The environment is in the process of being deleted.
    --
    --
    --     * @DELETE_FAILED@ : The environment failed to delete.
    status :: Core.Maybe Types.EnvironmentLifecycleStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentLifecycle' value with any optional fields omitted.
mkEnvironmentLifecycle ::
  EnvironmentLifecycle
mkEnvironmentLifecycle =
  EnvironmentLifecycle'
    { failureResource = Core.Nothing,
      reason = Core.Nothing,
      status = Core.Nothing
    }

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
--
-- /Note:/ Consider using 'failureResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elFailureResource :: Lens.Lens' EnvironmentLifecycle (Core.Maybe Types.String)
elFailureResource = Lens.field @"failureResource"
{-# DEPRECATED elFailureResource "Use generic-lens or generic-optics with 'failureResource' instead." #-}

-- | Any informational message about the lifecycle state of the environment.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elReason :: Lens.Lens' EnvironmentLifecycle (Core.Maybe Types.String)
elReason = Lens.field @"reason"
{-# DEPRECATED elReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The current creation or deletion lifecycle state of the environment.
--
--
--     * @CREATING@ : The environment is in the process of being created.
--
--
--     * @CREATED@ : The environment was successfully created.
--
--
--     * @CREATE_FAILED@ : The environment failed to be created.
--
--
--     * @DELETING@ : The environment is in the process of being deleted.
--
--
--     * @DELETE_FAILED@ : The environment failed to delete.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elStatus :: Lens.Lens' EnvironmentLifecycle (Core.Maybe Types.EnvironmentLifecycleStatus)
elStatus = Lens.field @"status"
{-# DEPRECATED elStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON EnvironmentLifecycle where
  parseJSON =
    Core.withObject "EnvironmentLifecycle" Core.$
      \x ->
        EnvironmentLifecycle'
          Core.<$> (x Core..:? "failureResource")
          Core.<*> (x Core..:? "reason")
          Core.<*> (x Core..:? "status")

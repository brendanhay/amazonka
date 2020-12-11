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
    elStatus,
    elFailureResource,
    elReason,
  )
where

import Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the current creation or deletion lifecycle state of an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { status ::
      Lude.Maybe EnvironmentLifecycleStatus,
    failureResource :: Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentLifecycle' with the minimum fields required to make a request.
--
-- * 'failureResource' - If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
-- * 'reason' - Any informational message about the lifecycle state of the environment.
-- * 'status' - The current creation or deletion lifecycle state of the environment.
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
mkEnvironmentLifecycle ::
  EnvironmentLifecycle
mkEnvironmentLifecycle =
  EnvironmentLifecycle'
    { status = Lude.Nothing,
      failureResource = Lude.Nothing,
      reason = Lude.Nothing
    }

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
elStatus :: Lens.Lens' EnvironmentLifecycle (Lude.Maybe EnvironmentLifecycleStatus)
elStatus = Lens.lens (status :: EnvironmentLifecycle -> Lude.Maybe EnvironmentLifecycleStatus) (\s a -> s {status = a} :: EnvironmentLifecycle)
{-# DEPRECATED elStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
--
-- /Note:/ Consider using 'failureResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elFailureResource :: Lens.Lens' EnvironmentLifecycle (Lude.Maybe Lude.Text)
elFailureResource = Lens.lens (failureResource :: EnvironmentLifecycle -> Lude.Maybe Lude.Text) (\s a -> s {failureResource = a} :: EnvironmentLifecycle)
{-# DEPRECATED elFailureResource "Use generic-lens or generic-optics with 'failureResource' instead." #-}

-- | Any informational message about the lifecycle state of the environment.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elReason :: Lens.Lens' EnvironmentLifecycle (Lude.Maybe Lude.Text)
elReason = Lens.lens (reason :: EnvironmentLifecycle -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: EnvironmentLifecycle)
{-# DEPRECATED elReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.FromJSON EnvironmentLifecycle where
  parseJSON =
    Lude.withObject
      "EnvironmentLifecycle"
      ( \x ->
          EnvironmentLifecycle'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "failureResource")
            Lude.<*> (x Lude..:? "reason")
      )

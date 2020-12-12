{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerStateChange
  ( ContainerStateChange (..),

    -- * Smart constructor
    mkContainerStateChange,

    -- * Lenses
    cscNetworkBindings,
    cscStatus,
    cscContainerName,
    cscReason,
    cscImageDigest,
    cscExitCode,
    cscRuntimeId,
  )
where

import Network.AWS.ECS.Types.NetworkBinding
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a change in state for a container.
--
-- /See:/ 'mkContainerStateChange' smart constructor.
data ContainerStateChange = ContainerStateChange'
  { networkBindings ::
      Lude.Maybe [NetworkBinding],
    status :: Lude.Maybe Lude.Text,
    containerName :: Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text,
    imageDigest :: Lude.Maybe Lude.Text,
    exitCode :: Lude.Maybe Lude.Int,
    runtimeId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerStateChange' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container.
-- * 'exitCode' - The exit code for the container, if the state change is a result of the container exiting.
-- * 'imageDigest' - The container image SHA 256 digest.
-- * 'networkBindings' - Any network bindings associated with the container.
-- * 'reason' - The reason for the state change.
-- * 'runtimeId' - The ID of the Docker container.
-- * 'status' - The status of the container.
mkContainerStateChange ::
  ContainerStateChange
mkContainerStateChange =
  ContainerStateChange'
    { networkBindings = Lude.Nothing,
      status = Lude.Nothing,
      containerName = Lude.Nothing,
      reason = Lude.Nothing,
      imageDigest = Lude.Nothing,
      exitCode = Lude.Nothing,
      runtimeId = Lude.Nothing
    }

-- | Any network bindings associated with the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscNetworkBindings :: Lens.Lens' ContainerStateChange (Lude.Maybe [NetworkBinding])
cscNetworkBindings = Lens.lens (networkBindings :: ContainerStateChange -> Lude.Maybe [NetworkBinding]) (\s a -> s {networkBindings = a} :: ContainerStateChange)
{-# DEPRECATED cscNetworkBindings "Use generic-lens or generic-optics with 'networkBindings' instead." #-}

-- | The status of the container.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscStatus :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Text)
cscStatus = Lens.lens (status :: ContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ContainerStateChange)
{-# DEPRECATED cscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscContainerName :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Text)
cscContainerName = Lens.lens (containerName :: ContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: ContainerStateChange)
{-# DEPRECATED cscContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The reason for the state change.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscReason :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Text)
cscReason = Lens.lens (reason :: ContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ContainerStateChange)
{-# DEPRECATED cscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The container image SHA 256 digest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscImageDigest :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Text)
cscImageDigest = Lens.lens (imageDigest :: ContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: ContainerStateChange)
{-# DEPRECATED cscImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The exit code for the container, if the state change is a result of the container exiting.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscExitCode :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Int)
cscExitCode = Lens.lens (exitCode :: ContainerStateChange -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: ContainerStateChange)
{-# DEPRECATED cscExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscRuntimeId :: Lens.Lens' ContainerStateChange (Lude.Maybe Lude.Text)
cscRuntimeId = Lens.lens (runtimeId :: ContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {runtimeId = a} :: ContainerStateChange)
{-# DEPRECATED cscRuntimeId "Use generic-lens or generic-optics with 'runtimeId' instead." #-}

instance Lude.ToJSON ContainerStateChange where
  toJSON ContainerStateChange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("networkBindings" Lude..=) Lude.<$> networkBindings,
            ("status" Lude..=) Lude.<$> status,
            ("containerName" Lude..=) Lude.<$> containerName,
            ("reason" Lude..=) Lude.<$> reason,
            ("imageDigest" Lude..=) Lude.<$> imageDigest,
            ("exitCode" Lude..=) Lude.<$> exitCode,
            ("runtimeId" Lude..=) Lude.<$> runtimeId
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dTargetARN,
    dCreationTime,
    dArn,
    dAccessPolicy,
    dDestinationName,
    dRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a cross-account destination that receives subscription log events.
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { targetARN :: Lude.Maybe Lude.Text,
    creationTime :: Lude.Maybe Lude.Natural,
    arn :: Lude.Maybe Lude.Text,
    accessPolicy :: Lude.Maybe Lude.Text,
    destinationName :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- * 'accessPolicy' - An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
-- * 'arn' - The ARN of this destination.
-- * 'creationTime' - The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'destinationName' - The name of the destination.
-- * 'roleARN' - A role for impersonation, used when delivering log events to the target.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
mkDestination ::
  Destination
mkDestination =
  Destination'
    { targetARN = Lude.Nothing,
      creationTime = Lude.Nothing,
      arn = Lude.Nothing,
      accessPolicy = Lude.Nothing,
      destinationName = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetARN :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dTargetARN = Lens.lens (targetARN :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: Destination)
{-# DEPRECATED dTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationTime :: Lens.Lens' Destination (Lude.Maybe Lude.Natural)
dCreationTime = Lens.lens (creationTime :: Destination -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: Destination)
{-# DEPRECATED dCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ARN of this destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dArn = Lens.lens (arn :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Destination)
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
--
-- /Note:/ Consider using 'accessPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessPolicy :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dAccessPolicy = Lens.lens (accessPolicy :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {accessPolicy = a} :: Destination)
{-# DEPRECATED dAccessPolicy "Use generic-lens or generic-optics with 'accessPolicy' instead." #-}

-- | The name of the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationName :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dDestinationName = Lens.lens (destinationName :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {destinationName = a} :: Destination)
{-# DEPRECATED dDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

-- | A role for impersonation, used when delivering log events to the target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleARN :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dRoleARN = Lens.lens (roleARN :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Destination)
{-# DEPRECATED dRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON Destination where
  parseJSON =
    Lude.withObject
      "Destination"
      ( \x ->
          Destination'
            Lude.<$> (x Lude..:? "targetArn")
            Lude.<*> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "accessPolicy")
            Lude.<*> (x Lude..:? "destinationName")
            Lude.<*> (x Lude..:? "roleArn")
      )

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Pose
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Pose where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
--
-- /See:/ 'newPose' smart constructor.
data Pose = Pose'
  { -- | Value representing the face rotation on the yaw axis.
    yaw :: Core.Maybe Core.Double,
    -- | Value representing the face rotation on the pitch axis.
    pitch :: Core.Maybe Core.Double,
    -- | Value representing the face rotation on the roll axis.
    roll :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Pose' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'yaw', 'pose_yaw' - Value representing the face rotation on the yaw axis.
--
-- 'pitch', 'pose_pitch' - Value representing the face rotation on the pitch axis.
--
-- 'roll', 'pose_roll' - Value representing the face rotation on the roll axis.
newPose ::
  Pose
newPose =
  Pose'
    { yaw = Core.Nothing,
      pitch = Core.Nothing,
      roll = Core.Nothing
    }

-- | Value representing the face rotation on the yaw axis.
pose_yaw :: Lens.Lens' Pose (Core.Maybe Core.Double)
pose_yaw = Lens.lens (\Pose' {yaw} -> yaw) (\s@Pose' {} a -> s {yaw = a} :: Pose)

-- | Value representing the face rotation on the pitch axis.
pose_pitch :: Lens.Lens' Pose (Core.Maybe Core.Double)
pose_pitch = Lens.lens (\Pose' {pitch} -> pitch) (\s@Pose' {} a -> s {pitch = a} :: Pose)

-- | Value representing the face rotation on the roll axis.
pose_roll :: Lens.Lens' Pose (Core.Maybe Core.Double)
pose_roll = Lens.lens (\Pose' {roll} -> roll) (\s@Pose' {} a -> s {roll = a} :: Pose)

instance Core.FromJSON Pose where
  parseJSON =
    Core.withObject
      "Pose"
      ( \x ->
          Pose'
            Core.<$> (x Core..:? "Yaw")
            Core.<*> (x Core..:? "Pitch")
            Core.<*> (x Core..:? "Roll")
      )

instance Core.Hashable Pose

instance Core.NFData Pose

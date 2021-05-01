{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
--
-- /See:/ 'newPose' smart constructor.
data Pose = Pose'
  { -- | Value representing the face rotation on the yaw axis.
    yaw :: Prelude.Maybe Prelude.Double,
    -- | Value representing the face rotation on the pitch axis.
    pitch :: Prelude.Maybe Prelude.Double,
    -- | Value representing the face rotation on the roll axis.
    roll :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { yaw = Prelude.Nothing,
      pitch = Prelude.Nothing,
      roll = Prelude.Nothing
    }

-- | Value representing the face rotation on the yaw axis.
pose_yaw :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_yaw = Lens.lens (\Pose' {yaw} -> yaw) (\s@Pose' {} a -> s {yaw = a} :: Pose)

-- | Value representing the face rotation on the pitch axis.
pose_pitch :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_pitch = Lens.lens (\Pose' {pitch} -> pitch) (\s@Pose' {} a -> s {pitch = a} :: Pose)

-- | Value representing the face rotation on the roll axis.
pose_roll :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_roll = Lens.lens (\Pose' {roll} -> roll) (\s@Pose' {} a -> s {roll = a} :: Pose)

instance Prelude.FromJSON Pose where
  parseJSON =
    Prelude.withObject
      "Pose"
      ( \x ->
          Pose'
            Prelude.<$> (x Prelude..:? "Yaw")
            Prelude.<*> (x Prelude..:? "Pitch")
            Prelude.<*> (x Prelude..:? "Roll")
      )

instance Prelude.Hashable Pose

instance Prelude.NFData Pose

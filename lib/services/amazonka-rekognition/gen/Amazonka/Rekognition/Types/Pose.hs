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
-- Module      : Amazonka.Rekognition.Types.Pose
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Pose where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
--
-- /See:/ 'newPose' smart constructor.
data Pose = Pose'
  { -- | Value representing the face rotation on the pitch axis.
    pitch :: Prelude.Maybe Prelude.Double,
    -- | Value representing the face rotation on the roll axis.
    roll :: Prelude.Maybe Prelude.Double,
    -- | Value representing the face rotation on the yaw axis.
    yaw :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pose' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pitch', 'pose_pitch' - Value representing the face rotation on the pitch axis.
--
-- 'roll', 'pose_roll' - Value representing the face rotation on the roll axis.
--
-- 'yaw', 'pose_yaw' - Value representing the face rotation on the yaw axis.
newPose ::
  Pose
newPose =
  Pose'
    { pitch = Prelude.Nothing,
      roll = Prelude.Nothing,
      yaw = Prelude.Nothing
    }

-- | Value representing the face rotation on the pitch axis.
pose_pitch :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_pitch = Lens.lens (\Pose' {pitch} -> pitch) (\s@Pose' {} a -> s {pitch = a} :: Pose)

-- | Value representing the face rotation on the roll axis.
pose_roll :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_roll = Lens.lens (\Pose' {roll} -> roll) (\s@Pose' {} a -> s {roll = a} :: Pose)

-- | Value representing the face rotation on the yaw axis.
pose_yaw :: Lens.Lens' Pose (Prelude.Maybe Prelude.Double)
pose_yaw = Lens.lens (\Pose' {yaw} -> yaw) (\s@Pose' {} a -> s {yaw = a} :: Pose)

instance Data.FromJSON Pose where
  parseJSON =
    Data.withObject
      "Pose"
      ( \x ->
          Pose'
            Prelude.<$> (x Data..:? "Pitch")
            Prelude.<*> (x Data..:? "Roll")
            Prelude.<*> (x Data..:? "Yaw")
      )

instance Prelude.Hashable Pose where
  hashWithSalt _salt Pose' {..} =
    _salt `Prelude.hashWithSalt` pitch
      `Prelude.hashWithSalt` roll
      `Prelude.hashWithSalt` yaw

instance Prelude.NFData Pose where
  rnf Pose' {..} =
    Prelude.rnf pitch
      `Prelude.seq` Prelude.rnf roll
      `Prelude.seq` Prelude.rnf yaw

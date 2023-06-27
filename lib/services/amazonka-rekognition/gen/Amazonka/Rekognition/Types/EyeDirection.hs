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
-- Module      : Amazonka.Rekognition.Types.EyeDirection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EyeDirection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the direction the eyes are gazing in (independent of the head
-- pose) as determined by its pitch and yaw.
--
-- /See:/ 'newEyeDirection' smart constructor.
data EyeDirection = EyeDirection'
  { -- | The confidence that the service has in its predicted eye direction.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Value representing eye direction on the pitch axis.
    pitch :: Prelude.Maybe Prelude.Double,
    -- | Value representing eye direction on the yaw axis.
    yaw :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EyeDirection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'eyeDirection_confidence' - The confidence that the service has in its predicted eye direction.
--
-- 'pitch', 'eyeDirection_pitch' - Value representing eye direction on the pitch axis.
--
-- 'yaw', 'eyeDirection_yaw' - Value representing eye direction on the yaw axis.
newEyeDirection ::
  EyeDirection
newEyeDirection =
  EyeDirection'
    { confidence = Prelude.Nothing,
      pitch = Prelude.Nothing,
      yaw = Prelude.Nothing
    }

-- | The confidence that the service has in its predicted eye direction.
eyeDirection_confidence :: Lens.Lens' EyeDirection (Prelude.Maybe Prelude.Double)
eyeDirection_confidence = Lens.lens (\EyeDirection' {confidence} -> confidence) (\s@EyeDirection' {} a -> s {confidence = a} :: EyeDirection)

-- | Value representing eye direction on the pitch axis.
eyeDirection_pitch :: Lens.Lens' EyeDirection (Prelude.Maybe Prelude.Double)
eyeDirection_pitch = Lens.lens (\EyeDirection' {pitch} -> pitch) (\s@EyeDirection' {} a -> s {pitch = a} :: EyeDirection)

-- | Value representing eye direction on the yaw axis.
eyeDirection_yaw :: Lens.Lens' EyeDirection (Prelude.Maybe Prelude.Double)
eyeDirection_yaw = Lens.lens (\EyeDirection' {yaw} -> yaw) (\s@EyeDirection' {} a -> s {yaw = a} :: EyeDirection)

instance Data.FromJSON EyeDirection where
  parseJSON =
    Data.withObject
      "EyeDirection"
      ( \x ->
          EyeDirection'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Pitch")
            Prelude.<*> (x Data..:? "Yaw")
      )

instance Prelude.Hashable EyeDirection where
  hashWithSalt _salt EyeDirection' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` pitch
      `Prelude.hashWithSalt` yaw

instance Prelude.NFData EyeDirection where
  rnf EyeDirection' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf pitch
      `Prelude.seq` Prelude.rnf yaw

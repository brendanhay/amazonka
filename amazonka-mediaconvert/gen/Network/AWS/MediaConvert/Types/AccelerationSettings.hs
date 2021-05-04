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
-- Module      : Network.AWS.MediaConvert.Types.AccelerationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AccelerationMode
import qualified Network.AWS.Prelude as Prelude

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
--
-- /See:/ 'newAccelerationSettings' smart constructor.
data AccelerationSettings = AccelerationSettings'
  { -- | Specify the conditions when the service will run your job with
    -- accelerated transcoding.
    mode :: AccelerationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccelerationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'accelerationSettings_mode' - Specify the conditions when the service will run your job with
-- accelerated transcoding.
newAccelerationSettings ::
  -- | 'mode'
  AccelerationMode ->
  AccelerationSettings
newAccelerationSettings pMode_ =
  AccelerationSettings' {mode = pMode_}

-- | Specify the conditions when the service will run your job with
-- accelerated transcoding.
accelerationSettings_mode :: Lens.Lens' AccelerationSettings AccelerationMode
accelerationSettings_mode = Lens.lens (\AccelerationSettings' {mode} -> mode) (\s@AccelerationSettings' {} a -> s {mode = a} :: AccelerationSettings)

instance Prelude.FromJSON AccelerationSettings where
  parseJSON =
    Prelude.withObject
      "AccelerationSettings"
      ( \x ->
          AccelerationSettings'
            Prelude.<$> (x Prelude..: "mode")
      )

instance Prelude.Hashable AccelerationSettings

instance Prelude.NFData AccelerationSettings

instance Prelude.ToJSON AccelerationSettings where
  toJSON AccelerationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("mode" Prelude..= mode)]
      )

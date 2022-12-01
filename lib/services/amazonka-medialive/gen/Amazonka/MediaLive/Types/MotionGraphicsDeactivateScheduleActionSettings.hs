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
-- Module      : Amazonka.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings to specify the ending of rendering motion graphics into the
-- video stream.
--
-- /See:/ 'newMotionGraphicsDeactivateScheduleActionSettings' smart constructor.
data MotionGraphicsDeactivateScheduleActionSettings = MotionGraphicsDeactivateScheduleActionSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MotionGraphicsDeactivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newMotionGraphicsDeactivateScheduleActionSettings ::
  MotionGraphicsDeactivateScheduleActionSettings
newMotionGraphicsDeactivateScheduleActionSettings =
  MotionGraphicsDeactivateScheduleActionSettings'

instance
  Core.FromJSON
    MotionGraphicsDeactivateScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "MotionGraphicsDeactivateScheduleActionSettings"
      ( \x ->
          Prelude.pure
            MotionGraphicsDeactivateScheduleActionSettings'
      )

instance
  Prelude.Hashable
    MotionGraphicsDeactivateScheduleActionSettings
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    MotionGraphicsDeactivateScheduleActionSettings
  where
  rnf _ = ()

instance
  Core.ToJSON
    MotionGraphicsDeactivateScheduleActionSettings
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

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
-- Module      : Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'newImmediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImmediateModeScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newImmediateModeScheduleActionStartSettings ::
  ImmediateModeScheduleActionStartSettings
newImmediateModeScheduleActionStartSettings =
  ImmediateModeScheduleActionStartSettings'

instance
  Core.FromJSON
    ImmediateModeScheduleActionStartSettings
  where
  parseJSON =
    Core.withObject
      "ImmediateModeScheduleActionStartSettings"
      ( \x ->
          Core.pure ImmediateModeScheduleActionStartSettings'
      )

instance
  Core.Hashable
    ImmediateModeScheduleActionStartSettings

instance
  Core.NFData
    ImmediateModeScheduleActionStartSettings

instance
  Core.ToJSON
    ImmediateModeScheduleActionStartSettings
  where
  toJSON = Core.const (Core.Object Core.mempty)

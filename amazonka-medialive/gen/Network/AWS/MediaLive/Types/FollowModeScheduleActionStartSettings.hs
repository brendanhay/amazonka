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
-- Module      : Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FollowPoint

-- | Settings to specify if an action follows another.
--
-- /See:/ 'newFollowModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'
  { -- | The action name of another action that this one refers to.
    referenceActionName :: Core.Text,
    -- | Identifies whether this action starts relative to the start or relative
    -- to the end of the reference action.
    followPoint :: FollowPoint
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FollowModeScheduleActionStartSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceActionName', 'followModeScheduleActionStartSettings_referenceActionName' - The action name of another action that this one refers to.
--
-- 'followPoint', 'followModeScheduleActionStartSettings_followPoint' - Identifies whether this action starts relative to the start or relative
-- to the end of the reference action.
newFollowModeScheduleActionStartSettings ::
  -- | 'referenceActionName'
  Core.Text ->
  -- | 'followPoint'
  FollowPoint ->
  FollowModeScheduleActionStartSettings
newFollowModeScheduleActionStartSettings
  pReferenceActionName_
  pFollowPoint_ =
    FollowModeScheduleActionStartSettings'
      { referenceActionName =
          pReferenceActionName_,
        followPoint = pFollowPoint_
      }

-- | The action name of another action that this one refers to.
followModeScheduleActionStartSettings_referenceActionName :: Lens.Lens' FollowModeScheduleActionStartSettings Core.Text
followModeScheduleActionStartSettings_referenceActionName = Lens.lens (\FollowModeScheduleActionStartSettings' {referenceActionName} -> referenceActionName) (\s@FollowModeScheduleActionStartSettings' {} a -> s {referenceActionName = a} :: FollowModeScheduleActionStartSettings)

-- | Identifies whether this action starts relative to the start or relative
-- to the end of the reference action.
followModeScheduleActionStartSettings_followPoint :: Lens.Lens' FollowModeScheduleActionStartSettings FollowPoint
followModeScheduleActionStartSettings_followPoint = Lens.lens (\FollowModeScheduleActionStartSettings' {followPoint} -> followPoint) (\s@FollowModeScheduleActionStartSettings' {} a -> s {followPoint = a} :: FollowModeScheduleActionStartSettings)

instance
  Core.FromJSON
    FollowModeScheduleActionStartSettings
  where
  parseJSON =
    Core.withObject
      "FollowModeScheduleActionStartSettings"
      ( \x ->
          FollowModeScheduleActionStartSettings'
            Core.<$> (x Core..: "referenceActionName")
            Core.<*> (x Core..: "followPoint")
      )

instance
  Core.Hashable
    FollowModeScheduleActionStartSettings

instance
  Core.NFData
    FollowModeScheduleActionStartSettings

instance
  Core.ToJSON
    FollowModeScheduleActionStartSettings
  where
  toJSON FollowModeScheduleActionStartSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("referenceActionName" Core..= referenceActionName),
            Core.Just ("followPoint" Core..= followPoint)
          ]
      )

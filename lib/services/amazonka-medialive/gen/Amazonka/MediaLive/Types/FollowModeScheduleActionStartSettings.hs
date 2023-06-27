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
-- Module      : Amazonka.MediaLive.Types.FollowModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FollowModeScheduleActionStartSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FollowPoint
import qualified Amazonka.Prelude as Prelude

-- | Settings to specify if an action follows another.
--
-- /See:/ 'newFollowModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'
  { -- | The action name of another action that this one refers to.
    referenceActionName :: Prelude.Text,
    -- | Identifies whether this action starts relative to the start or relative
    -- to the end of the reference action.
    followPoint :: FollowPoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
followModeScheduleActionStartSettings_referenceActionName :: Lens.Lens' FollowModeScheduleActionStartSettings Prelude.Text
followModeScheduleActionStartSettings_referenceActionName = Lens.lens (\FollowModeScheduleActionStartSettings' {referenceActionName} -> referenceActionName) (\s@FollowModeScheduleActionStartSettings' {} a -> s {referenceActionName = a} :: FollowModeScheduleActionStartSettings)

-- | Identifies whether this action starts relative to the start or relative
-- to the end of the reference action.
followModeScheduleActionStartSettings_followPoint :: Lens.Lens' FollowModeScheduleActionStartSettings FollowPoint
followModeScheduleActionStartSettings_followPoint = Lens.lens (\FollowModeScheduleActionStartSettings' {followPoint} -> followPoint) (\s@FollowModeScheduleActionStartSettings' {} a -> s {followPoint = a} :: FollowModeScheduleActionStartSettings)

instance
  Data.FromJSON
    FollowModeScheduleActionStartSettings
  where
  parseJSON =
    Data.withObject
      "FollowModeScheduleActionStartSettings"
      ( \x ->
          FollowModeScheduleActionStartSettings'
            Prelude.<$> (x Data..: "referenceActionName")
            Prelude.<*> (x Data..: "followPoint")
      )

instance
  Prelude.Hashable
    FollowModeScheduleActionStartSettings
  where
  hashWithSalt
    _salt
    FollowModeScheduleActionStartSettings' {..} =
      _salt
        `Prelude.hashWithSalt` referenceActionName
        `Prelude.hashWithSalt` followPoint

instance
  Prelude.NFData
    FollowModeScheduleActionStartSettings
  where
  rnf FollowModeScheduleActionStartSettings' {..} =
    Prelude.rnf referenceActionName
      `Prelude.seq` Prelude.rnf followPoint

instance
  Data.ToJSON
    FollowModeScheduleActionStartSettings
  where
  toJSON FollowModeScheduleActionStartSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("referenceActionName" Data..= referenceActionName),
            Prelude.Just ("followPoint" Data..= followPoint)
          ]
      )

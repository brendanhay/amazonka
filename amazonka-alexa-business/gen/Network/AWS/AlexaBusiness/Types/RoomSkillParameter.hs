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
-- Module      : Network.AWS.AlexaBusiness.Types.RoomSkillParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomSkillParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A skill parameter associated with a room.
--
-- /See:/ 'newRoomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { -- | The parameter key of a room skill parameter. ParameterKey is an
    -- enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
    parameterKey :: Core.Text,
    -- | The parameter value of a room skill parameter.
    parameterValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoomSkillParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterKey', 'roomSkillParameter_parameterKey' - The parameter key of a room skill parameter. ParameterKey is an
-- enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- 'parameterValue', 'roomSkillParameter_parameterValue' - The parameter value of a room skill parameter.
newRoomSkillParameter ::
  -- | 'parameterKey'
  Core.Text ->
  -- | 'parameterValue'
  Core.Text ->
  RoomSkillParameter
newRoomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    { parameterKey = pParameterKey_,
      parameterValue = pParameterValue_
    }

-- | The parameter key of a room skill parameter. ParameterKey is an
-- enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
roomSkillParameter_parameterKey :: Lens.Lens' RoomSkillParameter Core.Text
roomSkillParameter_parameterKey = Lens.lens (\RoomSkillParameter' {parameterKey} -> parameterKey) (\s@RoomSkillParameter' {} a -> s {parameterKey = a} :: RoomSkillParameter)

-- | The parameter value of a room skill parameter.
roomSkillParameter_parameterValue :: Lens.Lens' RoomSkillParameter Core.Text
roomSkillParameter_parameterValue = Lens.lens (\RoomSkillParameter' {parameterValue} -> parameterValue) (\s@RoomSkillParameter' {} a -> s {parameterValue = a} :: RoomSkillParameter)

instance Core.FromJSON RoomSkillParameter where
  parseJSON =
    Core.withObject
      "RoomSkillParameter"
      ( \x ->
          RoomSkillParameter'
            Core.<$> (x Core..: "ParameterKey")
            Core.<*> (x Core..: "ParameterValue")
      )

instance Core.Hashable RoomSkillParameter

instance Core.NFData RoomSkillParameter

instance Core.ToJSON RoomSkillParameter where
  toJSON RoomSkillParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterKey" Core..= parameterKey),
            Core.Just ("ParameterValue" Core..= parameterValue)
          ]
      )

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
-- Module      : Amazonka.AlexaBusiness.Types.RoomSkillParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.RoomSkillParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A skill parameter associated with a room.
--
-- /See:/ 'newRoomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { -- | The parameter key of a room skill parameter. ParameterKey is an
    -- enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
    parameterKey :: Prelude.Text,
    -- | The parameter value of a room skill parameter.
    parameterValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'parameterValue'
  Prelude.Text ->
  RoomSkillParameter
newRoomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    { parameterKey = pParameterKey_,
      parameterValue = pParameterValue_
    }

-- | The parameter key of a room skill parameter. ParameterKey is an
-- enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
roomSkillParameter_parameterKey :: Lens.Lens' RoomSkillParameter Prelude.Text
roomSkillParameter_parameterKey = Lens.lens (\RoomSkillParameter' {parameterKey} -> parameterKey) (\s@RoomSkillParameter' {} a -> s {parameterKey = a} :: RoomSkillParameter)

-- | The parameter value of a room skill parameter.
roomSkillParameter_parameterValue :: Lens.Lens' RoomSkillParameter Prelude.Text
roomSkillParameter_parameterValue = Lens.lens (\RoomSkillParameter' {parameterValue} -> parameterValue) (\s@RoomSkillParameter' {} a -> s {parameterValue = a} :: RoomSkillParameter)

instance Data.FromJSON RoomSkillParameter where
  parseJSON =
    Data.withObject
      "RoomSkillParameter"
      ( \x ->
          RoomSkillParameter'
            Prelude.<$> (x Data..: "ParameterKey")
            Prelude.<*> (x Data..: "ParameterValue")
      )

instance Prelude.Hashable RoomSkillParameter where
  hashWithSalt _salt RoomSkillParameter' {..} =
    _salt `Prelude.hashWithSalt` parameterKey
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData RoomSkillParameter where
  rnf RoomSkillParameter' {..} =
    Prelude.rnf parameterKey
      `Prelude.seq` Prelude.rnf parameterValue

instance Data.ToJSON RoomSkillParameter where
  toJSON RoomSkillParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ParameterKey" Data..= parameterKey),
            Prelude.Just
              ("ParameterValue" Data..= parameterValue)
          ]
      )

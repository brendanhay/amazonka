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
-- Module      : Amazonka.ChimeSdkVoice.Types.SipMediaApplicationAlexaSkillConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SipMediaApplicationAlexaSkillConfiguration where

import Amazonka.ChimeSdkVoice.Types.AlexaSkillStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSipMediaApplicationAlexaSkillConfiguration' smart constructor.
data SipMediaApplicationAlexaSkillConfiguration = SipMediaApplicationAlexaSkillConfiguration'
  { alexaSkillStatus :: AlexaSkillStatus,
    alexaSkillIds :: Prelude.NonEmpty (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipMediaApplicationAlexaSkillConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alexaSkillStatus', 'sipMediaApplicationAlexaSkillConfiguration_alexaSkillStatus' - Undocumented member.
--
-- 'alexaSkillIds', 'sipMediaApplicationAlexaSkillConfiguration_alexaSkillIds' - Undocumented member.
newSipMediaApplicationAlexaSkillConfiguration ::
  -- | 'alexaSkillStatus'
  AlexaSkillStatus ->
  -- | 'alexaSkillIds'
  Prelude.NonEmpty Prelude.Text ->
  SipMediaApplicationAlexaSkillConfiguration
newSipMediaApplicationAlexaSkillConfiguration
  pAlexaSkillStatus_
  pAlexaSkillIds_ =
    SipMediaApplicationAlexaSkillConfiguration'
      { alexaSkillStatus =
          pAlexaSkillStatus_,
        alexaSkillIds =
          Lens.coerced
            Lens.# pAlexaSkillIds_
      }

-- | Undocumented member.
sipMediaApplicationAlexaSkillConfiguration_alexaSkillStatus :: Lens.Lens' SipMediaApplicationAlexaSkillConfiguration AlexaSkillStatus
sipMediaApplicationAlexaSkillConfiguration_alexaSkillStatus = Lens.lens (\SipMediaApplicationAlexaSkillConfiguration' {alexaSkillStatus} -> alexaSkillStatus) (\s@SipMediaApplicationAlexaSkillConfiguration' {} a -> s {alexaSkillStatus = a} :: SipMediaApplicationAlexaSkillConfiguration)

-- | Undocumented member.
sipMediaApplicationAlexaSkillConfiguration_alexaSkillIds :: Lens.Lens' SipMediaApplicationAlexaSkillConfiguration (Prelude.NonEmpty Prelude.Text)
sipMediaApplicationAlexaSkillConfiguration_alexaSkillIds = Lens.lens (\SipMediaApplicationAlexaSkillConfiguration' {alexaSkillIds} -> alexaSkillIds) (\s@SipMediaApplicationAlexaSkillConfiguration' {} a -> s {alexaSkillIds = a} :: SipMediaApplicationAlexaSkillConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    SipMediaApplicationAlexaSkillConfiguration
  where
  parseJSON =
    Data.withObject
      "SipMediaApplicationAlexaSkillConfiguration"
      ( \x ->
          SipMediaApplicationAlexaSkillConfiguration'
            Prelude.<$> (x Data..: "AlexaSkillStatus")
            Prelude.<*> (x Data..: "AlexaSkillIds")
      )

instance
  Prelude.Hashable
    SipMediaApplicationAlexaSkillConfiguration
  where
  hashWithSalt
    _salt
    SipMediaApplicationAlexaSkillConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` alexaSkillStatus
        `Prelude.hashWithSalt` alexaSkillIds

instance
  Prelude.NFData
    SipMediaApplicationAlexaSkillConfiguration
  where
  rnf SipMediaApplicationAlexaSkillConfiguration' {..} =
    Prelude.rnf alexaSkillStatus
      `Prelude.seq` Prelude.rnf alexaSkillIds

instance
  Data.ToJSON
    SipMediaApplicationAlexaSkillConfiguration
  where
  toJSON
    SipMediaApplicationAlexaSkillConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("AlexaSkillStatus" Data..= alexaSkillStatus),
              Prelude.Just
                ("AlexaSkillIds" Data..= alexaSkillIds)
            ]
        )

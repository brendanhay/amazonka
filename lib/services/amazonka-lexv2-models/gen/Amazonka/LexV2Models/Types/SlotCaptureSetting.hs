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
-- Module      : Amazonka.LexV2Models.Types.SlotCaptureSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotCaptureSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Settings used when Amazon Lex successfully captures a slot value from a
-- user.
--
-- /See:/ 'newSlotCaptureSetting' smart constructor.
data SlotCaptureSetting = SlotCaptureSetting'
  { captureResponse :: Prelude.Maybe ResponseSpecification,
    -- | Specifies the next step that the bot runs when the slot value code is
    -- not recognized.
    failureNextStep :: Prelude.Maybe DialogState,
    -- | Code hook called after Amazon Lex successfully captures a slot value.
    codeHook :: Prelude.Maybe DialogCodeHookInvocationSetting,
    -- | Code hook called when Amazon Lex doesn\'t capture a slot value.
    elicitationCodeHook :: Prelude.Maybe ElicitationCodeHookInvocationSetting,
    -- | A list of conditional branches to evaluate after the slot value is
    -- captured.
    captureConditional :: Prelude.Maybe ConditionalSpecification,
    -- | A list of conditional branches to evaluate when the slot value isn\'t
    -- captured.
    failureConditional :: Prelude.Maybe ConditionalSpecification,
    failureResponse :: Prelude.Maybe ResponseSpecification,
    -- | Specifies the next step that the bot runs when the slot value is
    -- captured before the code hook times out.
    captureNextStep :: Prelude.Maybe DialogState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotCaptureSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureResponse', 'slotCaptureSetting_captureResponse' - Undocumented member.
--
-- 'failureNextStep', 'slotCaptureSetting_failureNextStep' - Specifies the next step that the bot runs when the slot value code is
-- not recognized.
--
-- 'codeHook', 'slotCaptureSetting_codeHook' - Code hook called after Amazon Lex successfully captures a slot value.
--
-- 'elicitationCodeHook', 'slotCaptureSetting_elicitationCodeHook' - Code hook called when Amazon Lex doesn\'t capture a slot value.
--
-- 'captureConditional', 'slotCaptureSetting_captureConditional' - A list of conditional branches to evaluate after the slot value is
-- captured.
--
-- 'failureConditional', 'slotCaptureSetting_failureConditional' - A list of conditional branches to evaluate when the slot value isn\'t
-- captured.
--
-- 'failureResponse', 'slotCaptureSetting_failureResponse' - Undocumented member.
--
-- 'captureNextStep', 'slotCaptureSetting_captureNextStep' - Specifies the next step that the bot runs when the slot value is
-- captured before the code hook times out.
newSlotCaptureSetting ::
  SlotCaptureSetting
newSlotCaptureSetting =
  SlotCaptureSetting'
    { captureResponse =
        Prelude.Nothing,
      failureNextStep = Prelude.Nothing,
      codeHook = Prelude.Nothing,
      elicitationCodeHook = Prelude.Nothing,
      captureConditional = Prelude.Nothing,
      failureConditional = Prelude.Nothing,
      failureResponse = Prelude.Nothing,
      captureNextStep = Prelude.Nothing
    }

-- | Undocumented member.
slotCaptureSetting_captureResponse :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe ResponseSpecification)
slotCaptureSetting_captureResponse = Lens.lens (\SlotCaptureSetting' {captureResponse} -> captureResponse) (\s@SlotCaptureSetting' {} a -> s {captureResponse = a} :: SlotCaptureSetting)

-- | Specifies the next step that the bot runs when the slot value code is
-- not recognized.
slotCaptureSetting_failureNextStep :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe DialogState)
slotCaptureSetting_failureNextStep = Lens.lens (\SlotCaptureSetting' {failureNextStep} -> failureNextStep) (\s@SlotCaptureSetting' {} a -> s {failureNextStep = a} :: SlotCaptureSetting)

-- | Code hook called after Amazon Lex successfully captures a slot value.
slotCaptureSetting_codeHook :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe DialogCodeHookInvocationSetting)
slotCaptureSetting_codeHook = Lens.lens (\SlotCaptureSetting' {codeHook} -> codeHook) (\s@SlotCaptureSetting' {} a -> s {codeHook = a} :: SlotCaptureSetting)

-- | Code hook called when Amazon Lex doesn\'t capture a slot value.
slotCaptureSetting_elicitationCodeHook :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe ElicitationCodeHookInvocationSetting)
slotCaptureSetting_elicitationCodeHook = Lens.lens (\SlotCaptureSetting' {elicitationCodeHook} -> elicitationCodeHook) (\s@SlotCaptureSetting' {} a -> s {elicitationCodeHook = a} :: SlotCaptureSetting)

-- | A list of conditional branches to evaluate after the slot value is
-- captured.
slotCaptureSetting_captureConditional :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe ConditionalSpecification)
slotCaptureSetting_captureConditional = Lens.lens (\SlotCaptureSetting' {captureConditional} -> captureConditional) (\s@SlotCaptureSetting' {} a -> s {captureConditional = a} :: SlotCaptureSetting)

-- | A list of conditional branches to evaluate when the slot value isn\'t
-- captured.
slotCaptureSetting_failureConditional :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe ConditionalSpecification)
slotCaptureSetting_failureConditional = Lens.lens (\SlotCaptureSetting' {failureConditional} -> failureConditional) (\s@SlotCaptureSetting' {} a -> s {failureConditional = a} :: SlotCaptureSetting)

-- | Undocumented member.
slotCaptureSetting_failureResponse :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe ResponseSpecification)
slotCaptureSetting_failureResponse = Lens.lens (\SlotCaptureSetting' {failureResponse} -> failureResponse) (\s@SlotCaptureSetting' {} a -> s {failureResponse = a} :: SlotCaptureSetting)

-- | Specifies the next step that the bot runs when the slot value is
-- captured before the code hook times out.
slotCaptureSetting_captureNextStep :: Lens.Lens' SlotCaptureSetting (Prelude.Maybe DialogState)
slotCaptureSetting_captureNextStep = Lens.lens (\SlotCaptureSetting' {captureNextStep} -> captureNextStep) (\s@SlotCaptureSetting' {} a -> s {captureNextStep = a} :: SlotCaptureSetting)

instance Data.FromJSON SlotCaptureSetting where
  parseJSON =
    Data.withObject
      "SlotCaptureSetting"
      ( \x ->
          SlotCaptureSetting'
            Prelude.<$> (x Data..:? "captureResponse")
            Prelude.<*> (x Data..:? "failureNextStep")
            Prelude.<*> (x Data..:? "codeHook")
            Prelude.<*> (x Data..:? "elicitationCodeHook")
            Prelude.<*> (x Data..:? "captureConditional")
            Prelude.<*> (x Data..:? "failureConditional")
            Prelude.<*> (x Data..:? "failureResponse")
            Prelude.<*> (x Data..:? "captureNextStep")
      )

instance Prelude.Hashable SlotCaptureSetting where
  hashWithSalt _salt SlotCaptureSetting' {..} =
    _salt `Prelude.hashWithSalt` captureResponse
      `Prelude.hashWithSalt` failureNextStep
      `Prelude.hashWithSalt` codeHook
      `Prelude.hashWithSalt` elicitationCodeHook
      `Prelude.hashWithSalt` captureConditional
      `Prelude.hashWithSalt` failureConditional
      `Prelude.hashWithSalt` failureResponse
      `Prelude.hashWithSalt` captureNextStep

instance Prelude.NFData SlotCaptureSetting where
  rnf SlotCaptureSetting' {..} =
    Prelude.rnf captureResponse
      `Prelude.seq` Prelude.rnf failureNextStep
      `Prelude.seq` Prelude.rnf codeHook
      `Prelude.seq` Prelude.rnf elicitationCodeHook
      `Prelude.seq` Prelude.rnf captureConditional
      `Prelude.seq` Prelude.rnf failureConditional
      `Prelude.seq` Prelude.rnf failureResponse
      `Prelude.seq` Prelude.rnf captureNextStep

instance Data.ToJSON SlotCaptureSetting where
  toJSON SlotCaptureSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("captureResponse" Data..=)
              Prelude.<$> captureResponse,
            ("failureNextStep" Data..=)
              Prelude.<$> failureNextStep,
            ("codeHook" Data..=) Prelude.<$> codeHook,
            ("elicitationCodeHook" Data..=)
              Prelude.<$> elicitationCodeHook,
            ("captureConditional" Data..=)
              Prelude.<$> captureConditional,
            ("failureConditional" Data..=)
              Prelude.<$> failureConditional,
            ("failureResponse" Data..=)
              Prelude.<$> failureResponse,
            ("captureNextStep" Data..=)
              Prelude.<$> captureNextStep
          ]
      )

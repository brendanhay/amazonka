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
-- Module      : Amazonka.LexV2Models.Types.SlotValueElicitationSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotValueElicitationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.PromptSpecification
import Amazonka.LexV2Models.Types.SampleUtterance
import Amazonka.LexV2Models.Types.SlotCaptureSetting
import Amazonka.LexV2Models.Types.SlotConstraint
import Amazonka.LexV2Models.Types.SlotDefaultValueSpecification
import Amazonka.LexV2Models.Types.WaitAndContinueSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies the elicitation setting details for constituent sub slots of a
-- composite slot.
--
-- /See:/ 'newSlotValueElicitationSetting' smart constructor.
data SlotValueElicitationSetting = SlotValueElicitationSetting'
  { -- | If you know a specific pattern that users might respond to an Amazon Lex
    -- request for a slot value, you can provide those utterances to improve
    -- accuracy. This is optional. In most cases, Amazon Lex is capable of
    -- understanding user utterances.
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    waitAndContinueSpecification :: Prelude.Maybe WaitAndContinueSpecification,
    -- | The prompt that Amazon Lex uses to elicit the slot value from the user.
    promptSpecification :: Prelude.Maybe PromptSpecification,
    -- | A list of default values for a slot. Default values are used when Amazon
    -- Lex hasn\'t determined a value for a slot. You can specify default
    -- values from context variables, session attributes, and defined values.
    defaultValueSpecification :: Prelude.Maybe SlotDefaultValueSpecification,
    -- | Specifies the settings that Amazon Lex uses when a slot value is
    -- successfully entered by a user.
    slotCaptureSetting :: Prelude.Maybe SlotCaptureSetting,
    -- | Specifies whether the slot is required or optional.
    slotConstraint :: SlotConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotValueElicitationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleUtterances', 'slotValueElicitationSetting_sampleUtterances' - If you know a specific pattern that users might respond to an Amazon Lex
-- request for a slot value, you can provide those utterances to improve
-- accuracy. This is optional. In most cases, Amazon Lex is capable of
-- understanding user utterances.
--
-- 'waitAndContinueSpecification', 'slotValueElicitationSetting_waitAndContinueSpecification' - Undocumented member.
--
-- 'promptSpecification', 'slotValueElicitationSetting_promptSpecification' - The prompt that Amazon Lex uses to elicit the slot value from the user.
--
-- 'defaultValueSpecification', 'slotValueElicitationSetting_defaultValueSpecification' - A list of default values for a slot. Default values are used when Amazon
-- Lex hasn\'t determined a value for a slot. You can specify default
-- values from context variables, session attributes, and defined values.
--
-- 'slotCaptureSetting', 'slotValueElicitationSetting_slotCaptureSetting' - Specifies the settings that Amazon Lex uses when a slot value is
-- successfully entered by a user.
--
-- 'slotConstraint', 'slotValueElicitationSetting_slotConstraint' - Specifies whether the slot is required or optional.
newSlotValueElicitationSetting ::
  -- | 'slotConstraint'
  SlotConstraint ->
  SlotValueElicitationSetting
newSlotValueElicitationSetting pSlotConstraint_ =
  SlotValueElicitationSetting'
    { sampleUtterances =
        Prelude.Nothing,
      waitAndContinueSpecification = Prelude.Nothing,
      promptSpecification = Prelude.Nothing,
      defaultValueSpecification = Prelude.Nothing,
      slotCaptureSetting = Prelude.Nothing,
      slotConstraint = pSlotConstraint_
    }

-- | If you know a specific pattern that users might respond to an Amazon Lex
-- request for a slot value, you can provide those utterances to improve
-- accuracy. This is optional. In most cases, Amazon Lex is capable of
-- understanding user utterances.
slotValueElicitationSetting_sampleUtterances :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe [SampleUtterance])
slotValueElicitationSetting_sampleUtterances = Lens.lens (\SlotValueElicitationSetting' {sampleUtterances} -> sampleUtterances) (\s@SlotValueElicitationSetting' {} a -> s {sampleUtterances = a} :: SlotValueElicitationSetting) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
slotValueElicitationSetting_waitAndContinueSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe WaitAndContinueSpecification)
slotValueElicitationSetting_waitAndContinueSpecification = Lens.lens (\SlotValueElicitationSetting' {waitAndContinueSpecification} -> waitAndContinueSpecification) (\s@SlotValueElicitationSetting' {} a -> s {waitAndContinueSpecification = a} :: SlotValueElicitationSetting)

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
slotValueElicitationSetting_promptSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe PromptSpecification)
slotValueElicitationSetting_promptSpecification = Lens.lens (\SlotValueElicitationSetting' {promptSpecification} -> promptSpecification) (\s@SlotValueElicitationSetting' {} a -> s {promptSpecification = a} :: SlotValueElicitationSetting)

-- | A list of default values for a slot. Default values are used when Amazon
-- Lex hasn\'t determined a value for a slot. You can specify default
-- values from context variables, session attributes, and defined values.
slotValueElicitationSetting_defaultValueSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe SlotDefaultValueSpecification)
slotValueElicitationSetting_defaultValueSpecification = Lens.lens (\SlotValueElicitationSetting' {defaultValueSpecification} -> defaultValueSpecification) (\s@SlotValueElicitationSetting' {} a -> s {defaultValueSpecification = a} :: SlotValueElicitationSetting)

-- | Specifies the settings that Amazon Lex uses when a slot value is
-- successfully entered by a user.
slotValueElicitationSetting_slotCaptureSetting :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe SlotCaptureSetting)
slotValueElicitationSetting_slotCaptureSetting = Lens.lens (\SlotValueElicitationSetting' {slotCaptureSetting} -> slotCaptureSetting) (\s@SlotValueElicitationSetting' {} a -> s {slotCaptureSetting = a} :: SlotValueElicitationSetting)

-- | Specifies whether the slot is required or optional.
slotValueElicitationSetting_slotConstraint :: Lens.Lens' SlotValueElicitationSetting SlotConstraint
slotValueElicitationSetting_slotConstraint = Lens.lens (\SlotValueElicitationSetting' {slotConstraint} -> slotConstraint) (\s@SlotValueElicitationSetting' {} a -> s {slotConstraint = a} :: SlotValueElicitationSetting)

instance Data.FromJSON SlotValueElicitationSetting where
  parseJSON =
    Data.withObject
      "SlotValueElicitationSetting"
      ( \x ->
          SlotValueElicitationSetting'
            Prelude.<$> ( x Data..:? "sampleUtterances"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "waitAndContinueSpecification")
            Prelude.<*> (x Data..:? "promptSpecification")
            Prelude.<*> (x Data..:? "defaultValueSpecification")
            Prelude.<*> (x Data..:? "slotCaptureSetting")
            Prelude.<*> (x Data..: "slotConstraint")
      )

instance Prelude.Hashable SlotValueElicitationSetting where
  hashWithSalt _salt SlotValueElicitationSetting' {..} =
    _salt `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` waitAndContinueSpecification
      `Prelude.hashWithSalt` promptSpecification
      `Prelude.hashWithSalt` defaultValueSpecification
      `Prelude.hashWithSalt` slotCaptureSetting
      `Prelude.hashWithSalt` slotConstraint

instance Prelude.NFData SlotValueElicitationSetting where
  rnf SlotValueElicitationSetting' {..} =
    Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf waitAndContinueSpecification
      `Prelude.seq` Prelude.rnf promptSpecification
      `Prelude.seq` Prelude.rnf defaultValueSpecification
      `Prelude.seq` Prelude.rnf slotCaptureSetting
      `Prelude.seq` Prelude.rnf slotConstraint

instance Data.ToJSON SlotValueElicitationSetting where
  toJSON SlotValueElicitationSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sampleUtterances" Data..=)
              Prelude.<$> sampleUtterances,
            ("waitAndContinueSpecification" Data..=)
              Prelude.<$> waitAndContinueSpecification,
            ("promptSpecification" Data..=)
              Prelude.<$> promptSpecification,
            ("defaultValueSpecification" Data..=)
              Prelude.<$> defaultValueSpecification,
            ("slotCaptureSetting" Data..=)
              Prelude.<$> slotCaptureSetting,
            Prelude.Just
              ("slotConstraint" Data..= slotConstraint)
          ]
      )

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
-- Module      : Network.AWS.LexV2Models.Types.SlotValueElicitationSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.SlotValueElicitationSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.PromptSpecification
import Network.AWS.LexV2Models.Types.SampleUtterance
import Network.AWS.LexV2Models.Types.SlotConstraint
import Network.AWS.LexV2Models.Types.SlotDefaultValueSpecification
import Network.AWS.LexV2Models.Types.WaitAndContinueSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Settings that you can use for eliciting a slot value.
--
-- /See:/ 'newSlotValueElicitationSetting' smart constructor.
data SlotValueElicitationSetting = SlotValueElicitationSetting'
  { waitAndContinueSpecification :: Prelude.Maybe WaitAndContinueSpecification,
    -- | A list of default values for a slot. Default values are used when Amazon
    -- Lex hasn\'t determined a value for a slot. You can specify default
    -- values from context variables, session attributes, and defined values.
    defaultValueSpecification :: Prelude.Maybe SlotDefaultValueSpecification,
    -- | If you know a specific pattern that users might respond to an Amazon Lex
    -- request for a slot value, you can provide those utterances to improve
    -- accuracy. This is optional. In most cases, Amazon Lex is capable of
    -- understanding user utterances.
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    -- | The prompt that Amazon Lex uses to elicit the slot value from the user.
    promptSpecification :: Prelude.Maybe PromptSpecification,
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
-- 'waitAndContinueSpecification', 'slotValueElicitationSetting_waitAndContinueSpecification' - Undocumented member.
--
-- 'defaultValueSpecification', 'slotValueElicitationSetting_defaultValueSpecification' - A list of default values for a slot. Default values are used when Amazon
-- Lex hasn\'t determined a value for a slot. You can specify default
-- values from context variables, session attributes, and defined values.
--
-- 'sampleUtterances', 'slotValueElicitationSetting_sampleUtterances' - If you know a specific pattern that users might respond to an Amazon Lex
-- request for a slot value, you can provide those utterances to improve
-- accuracy. This is optional. In most cases, Amazon Lex is capable of
-- understanding user utterances.
--
-- 'promptSpecification', 'slotValueElicitationSetting_promptSpecification' - The prompt that Amazon Lex uses to elicit the slot value from the user.
--
-- 'slotConstraint', 'slotValueElicitationSetting_slotConstraint' - Specifies whether the slot is required or optional.
newSlotValueElicitationSetting ::
  -- | 'slotConstraint'
  SlotConstraint ->
  SlotValueElicitationSetting
newSlotValueElicitationSetting pSlotConstraint_ =
  SlotValueElicitationSetting'
    { waitAndContinueSpecification =
        Prelude.Nothing,
      defaultValueSpecification = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      promptSpecification = Prelude.Nothing,
      slotConstraint = pSlotConstraint_
    }

-- | Undocumented member.
slotValueElicitationSetting_waitAndContinueSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe WaitAndContinueSpecification)
slotValueElicitationSetting_waitAndContinueSpecification = Lens.lens (\SlotValueElicitationSetting' {waitAndContinueSpecification} -> waitAndContinueSpecification) (\s@SlotValueElicitationSetting' {} a -> s {waitAndContinueSpecification = a} :: SlotValueElicitationSetting)

-- | A list of default values for a slot. Default values are used when Amazon
-- Lex hasn\'t determined a value for a slot. You can specify default
-- values from context variables, session attributes, and defined values.
slotValueElicitationSetting_defaultValueSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe SlotDefaultValueSpecification)
slotValueElicitationSetting_defaultValueSpecification = Lens.lens (\SlotValueElicitationSetting' {defaultValueSpecification} -> defaultValueSpecification) (\s@SlotValueElicitationSetting' {} a -> s {defaultValueSpecification = a} :: SlotValueElicitationSetting)

-- | If you know a specific pattern that users might respond to an Amazon Lex
-- request for a slot value, you can provide those utterances to improve
-- accuracy. This is optional. In most cases, Amazon Lex is capable of
-- understanding user utterances.
slotValueElicitationSetting_sampleUtterances :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe [SampleUtterance])
slotValueElicitationSetting_sampleUtterances = Lens.lens (\SlotValueElicitationSetting' {sampleUtterances} -> sampleUtterances) (\s@SlotValueElicitationSetting' {} a -> s {sampleUtterances = a} :: SlotValueElicitationSetting) Prelude.. Lens.mapping Lens.coerced

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
slotValueElicitationSetting_promptSpecification :: Lens.Lens' SlotValueElicitationSetting (Prelude.Maybe PromptSpecification)
slotValueElicitationSetting_promptSpecification = Lens.lens (\SlotValueElicitationSetting' {promptSpecification} -> promptSpecification) (\s@SlotValueElicitationSetting' {} a -> s {promptSpecification = a} :: SlotValueElicitationSetting)

-- | Specifies whether the slot is required or optional.
slotValueElicitationSetting_slotConstraint :: Lens.Lens' SlotValueElicitationSetting SlotConstraint
slotValueElicitationSetting_slotConstraint = Lens.lens (\SlotValueElicitationSetting' {slotConstraint} -> slotConstraint) (\s@SlotValueElicitationSetting' {} a -> s {slotConstraint = a} :: SlotValueElicitationSetting)

instance Core.FromJSON SlotValueElicitationSetting where
  parseJSON =
    Core.withObject
      "SlotValueElicitationSetting"
      ( \x ->
          SlotValueElicitationSetting'
            Prelude.<$> (x Core..:? "waitAndContinueSpecification")
            Prelude.<*> (x Core..:? "defaultValueSpecification")
            Prelude.<*> ( x Core..:? "sampleUtterances"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "promptSpecification")
            Prelude.<*> (x Core..: "slotConstraint")
      )

instance Prelude.Hashable SlotValueElicitationSetting

instance Prelude.NFData SlotValueElicitationSetting

instance Core.ToJSON SlotValueElicitationSetting where
  toJSON SlotValueElicitationSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("waitAndContinueSpecification" Core..=)
              Prelude.<$> waitAndContinueSpecification,
            ("defaultValueSpecification" Core..=)
              Prelude.<$> defaultValueSpecification,
            ("sampleUtterances" Core..=)
              Prelude.<$> sampleUtterances,
            ("promptSpecification" Core..=)
              Prelude.<$> promptSpecification,
            Prelude.Just
              ("slotConstraint" Core..= slotConstraint)
          ]
      )

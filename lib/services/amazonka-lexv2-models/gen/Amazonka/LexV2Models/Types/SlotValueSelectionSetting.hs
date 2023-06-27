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
-- Module      : Amazonka.LexV2Models.Types.SlotValueSelectionSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotValueSelectionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AdvancedRecognitionSetting
import Amazonka.LexV2Models.Types.SlotValueRegexFilter
import Amazonka.LexV2Models.Types.SlotValueResolutionStrategy
import qualified Amazonka.Prelude as Prelude

-- | Contains settings used by Amazon Lex to select a slot value.
--
-- /See:/ 'newSlotValueSelectionSetting' smart constructor.
data SlotValueSelectionSetting = SlotValueSelectionSetting'
  { -- | Provides settings that enable advanced recognition settings for slot
    -- values. You can use this to enable using slot values as a custom
    -- vocabulary for recognizing user utterances.
    advancedRecognitionSetting :: Prelude.Maybe AdvancedRecognitionSetting,
    -- | A regular expression used to validate the value of a slot.
    regexFilter :: Prelude.Maybe SlotValueRegexFilter,
    -- | Determines the slot resolution strategy that Amazon Lex uses to return
    -- slot type values. The field can be set to one of the following values:
    --
    -- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
    --     user value is similar to the slot value.
    --
    -- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
    --     return the first value in the resolution list as the slot type
    --     value. If there is no resolution list, null is returned.
    --
    -- If you don\'t specify the @valueSelectionStrategy@, the default is
    -- @ORIGINAL_VALUE@.
    resolutionStrategy :: SlotValueResolutionStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotValueSelectionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedRecognitionSetting', 'slotValueSelectionSetting_advancedRecognitionSetting' - Provides settings that enable advanced recognition settings for slot
-- values. You can use this to enable using slot values as a custom
-- vocabulary for recognizing user utterances.
--
-- 'regexFilter', 'slotValueSelectionSetting_regexFilter' - A regular expression used to validate the value of a slot.
--
-- 'resolutionStrategy', 'slotValueSelectionSetting_resolutionStrategy' - Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
--     user value is similar to the slot value.
--
-- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
--     return the first value in the resolution list as the slot type
--     value. If there is no resolution list, null is returned.
--
-- If you don\'t specify the @valueSelectionStrategy@, the default is
-- @ORIGINAL_VALUE@.
newSlotValueSelectionSetting ::
  -- | 'resolutionStrategy'
  SlotValueResolutionStrategy ->
  SlotValueSelectionSetting
newSlotValueSelectionSetting pResolutionStrategy_ =
  SlotValueSelectionSetting'
    { advancedRecognitionSetting =
        Prelude.Nothing,
      regexFilter = Prelude.Nothing,
      resolutionStrategy = pResolutionStrategy_
    }

-- | Provides settings that enable advanced recognition settings for slot
-- values. You can use this to enable using slot values as a custom
-- vocabulary for recognizing user utterances.
slotValueSelectionSetting_advancedRecognitionSetting :: Lens.Lens' SlotValueSelectionSetting (Prelude.Maybe AdvancedRecognitionSetting)
slotValueSelectionSetting_advancedRecognitionSetting = Lens.lens (\SlotValueSelectionSetting' {advancedRecognitionSetting} -> advancedRecognitionSetting) (\s@SlotValueSelectionSetting' {} a -> s {advancedRecognitionSetting = a} :: SlotValueSelectionSetting)

-- | A regular expression used to validate the value of a slot.
slotValueSelectionSetting_regexFilter :: Lens.Lens' SlotValueSelectionSetting (Prelude.Maybe SlotValueRegexFilter)
slotValueSelectionSetting_regexFilter = Lens.lens (\SlotValueSelectionSetting' {regexFilter} -> regexFilter) (\s@SlotValueSelectionSetting' {} a -> s {regexFilter = a} :: SlotValueSelectionSetting)

-- | Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
--     user value is similar to the slot value.
--
-- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
--     return the first value in the resolution list as the slot type
--     value. If there is no resolution list, null is returned.
--
-- If you don\'t specify the @valueSelectionStrategy@, the default is
-- @ORIGINAL_VALUE@.
slotValueSelectionSetting_resolutionStrategy :: Lens.Lens' SlotValueSelectionSetting SlotValueResolutionStrategy
slotValueSelectionSetting_resolutionStrategy = Lens.lens (\SlotValueSelectionSetting' {resolutionStrategy} -> resolutionStrategy) (\s@SlotValueSelectionSetting' {} a -> s {resolutionStrategy = a} :: SlotValueSelectionSetting)

instance Data.FromJSON SlotValueSelectionSetting where
  parseJSON =
    Data.withObject
      "SlotValueSelectionSetting"
      ( \x ->
          SlotValueSelectionSetting'
            Prelude.<$> (x Data..:? "advancedRecognitionSetting")
            Prelude.<*> (x Data..:? "regexFilter")
            Prelude.<*> (x Data..: "resolutionStrategy")
      )

instance Prelude.Hashable SlotValueSelectionSetting where
  hashWithSalt _salt SlotValueSelectionSetting' {..} =
    _salt
      `Prelude.hashWithSalt` advancedRecognitionSetting
      `Prelude.hashWithSalt` regexFilter
      `Prelude.hashWithSalt` resolutionStrategy

instance Prelude.NFData SlotValueSelectionSetting where
  rnf SlotValueSelectionSetting' {..} =
    Prelude.rnf advancedRecognitionSetting
      `Prelude.seq` Prelude.rnf regexFilter
      `Prelude.seq` Prelude.rnf resolutionStrategy

instance Data.ToJSON SlotValueSelectionSetting where
  toJSON SlotValueSelectionSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("advancedRecognitionSetting" Data..=)
              Prelude.<$> advancedRecognitionSetting,
            ("regexFilter" Data..=) Prelude.<$> regexFilter,
            Prelude.Just
              ("resolutionStrategy" Data..= resolutionStrategy)
          ]
      )

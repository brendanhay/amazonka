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
-- Module      : Network.AWS.LexV2Models.Types.SlotValueSelectionSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.SlotValueSelectionSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.SlotValueRegexFilter
import Network.AWS.LexV2Models.Types.SlotValueResolutionStrategy
import qualified Network.AWS.Prelude as Prelude

-- | Contains settings used by Amazon Lex to select a slot value.
--
-- /See:/ 'newSlotValueSelectionSetting' smart constructor.
data SlotValueSelectionSetting = SlotValueSelectionSetting'
  { -- | A regular expression used to validate the value of a slot.
    regexFilter :: Prelude.Maybe SlotValueRegexFilter,
    -- | Determines the slot resolution strategy that Amazon Lex uses to return
    -- slot type values. The field can be set to one of the following values:
    --
    -- -   OriginalValue - Returns the value entered by the user, if the user
    --     value is similar to the slot value.
    --
    -- -   TopResolution - If there is a resolution list for the slot, return
    --     the first value in the resolution list as the slot type value. If
    --     there is no resolution list, null is returned.
    --
    -- If you don\'t specify the valueSelectionStrategy, the default is
    -- OriginalValue.
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
-- 'regexFilter', 'slotValueSelectionSetting_regexFilter' - A regular expression used to validate the value of a slot.
--
-- 'resolutionStrategy', 'slotValueSelectionSetting_resolutionStrategy' - Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   OriginalValue - Returns the value entered by the user, if the user
--     value is similar to the slot value.
--
-- -   TopResolution - If there is a resolution list for the slot, return
--     the first value in the resolution list as the slot type value. If
--     there is no resolution list, null is returned.
--
-- If you don\'t specify the valueSelectionStrategy, the default is
-- OriginalValue.
newSlotValueSelectionSetting ::
  -- | 'resolutionStrategy'
  SlotValueResolutionStrategy ->
  SlotValueSelectionSetting
newSlotValueSelectionSetting pResolutionStrategy_ =
  SlotValueSelectionSetting'
    { regexFilter =
        Prelude.Nothing,
      resolutionStrategy = pResolutionStrategy_
    }

-- | A regular expression used to validate the value of a slot.
slotValueSelectionSetting_regexFilter :: Lens.Lens' SlotValueSelectionSetting (Prelude.Maybe SlotValueRegexFilter)
slotValueSelectionSetting_regexFilter = Lens.lens (\SlotValueSelectionSetting' {regexFilter} -> regexFilter) (\s@SlotValueSelectionSetting' {} a -> s {regexFilter = a} :: SlotValueSelectionSetting)

-- | Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   OriginalValue - Returns the value entered by the user, if the user
--     value is similar to the slot value.
--
-- -   TopResolution - If there is a resolution list for the slot, return
--     the first value in the resolution list as the slot type value. If
--     there is no resolution list, null is returned.
--
-- If you don\'t specify the valueSelectionStrategy, the default is
-- OriginalValue.
slotValueSelectionSetting_resolutionStrategy :: Lens.Lens' SlotValueSelectionSetting SlotValueResolutionStrategy
slotValueSelectionSetting_resolutionStrategy = Lens.lens (\SlotValueSelectionSetting' {resolutionStrategy} -> resolutionStrategy) (\s@SlotValueSelectionSetting' {} a -> s {resolutionStrategy = a} :: SlotValueSelectionSetting)

instance Core.FromJSON SlotValueSelectionSetting where
  parseJSON =
    Core.withObject
      "SlotValueSelectionSetting"
      ( \x ->
          SlotValueSelectionSetting'
            Prelude.<$> (x Core..:? "regexFilter")
            Prelude.<*> (x Core..: "resolutionStrategy")
      )

instance Prelude.Hashable SlotValueSelectionSetting

instance Prelude.NFData SlotValueSelectionSetting

instance Core.ToJSON SlotValueSelectionSetting where
  toJSON SlotValueSelectionSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("regexFilter" Core..=) Prelude.<$> regexFilter,
            Prelude.Just
              ("resolutionStrategy" Core..= resolutionStrategy)
          ]
      )

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
-- Module      : Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Locale

-- | Provides information about a built in slot type.
--
-- /See:/ 'newBuiltinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { -- | A unique identifier for the built-in slot type. To find the signature
    -- for a slot type, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
    -- in the /Alexa Skills Kit/.
    signature :: Core.Maybe Core.Text,
    -- | A list of target locales for the slot.
    supportedLocales :: Core.Maybe [Locale]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuiltinSlotTypeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'builtinSlotTypeMetadata_signature' - A unique identifier for the built-in slot type. To find the signature
-- for a slot type, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
--
-- 'supportedLocales', 'builtinSlotTypeMetadata_supportedLocales' - A list of target locales for the slot.
newBuiltinSlotTypeMetadata ::
  BuiltinSlotTypeMetadata
newBuiltinSlotTypeMetadata =
  BuiltinSlotTypeMetadata'
    { signature = Core.Nothing,
      supportedLocales = Core.Nothing
    }

-- | A unique identifier for the built-in slot type. To find the signature
-- for a slot type, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
builtinSlotTypeMetadata_signature :: Lens.Lens' BuiltinSlotTypeMetadata (Core.Maybe Core.Text)
builtinSlotTypeMetadata_signature = Lens.lens (\BuiltinSlotTypeMetadata' {signature} -> signature) (\s@BuiltinSlotTypeMetadata' {} a -> s {signature = a} :: BuiltinSlotTypeMetadata)

-- | A list of target locales for the slot.
builtinSlotTypeMetadata_supportedLocales :: Lens.Lens' BuiltinSlotTypeMetadata (Core.Maybe [Locale])
builtinSlotTypeMetadata_supportedLocales = Lens.lens (\BuiltinSlotTypeMetadata' {supportedLocales} -> supportedLocales) (\s@BuiltinSlotTypeMetadata' {} a -> s {supportedLocales = a} :: BuiltinSlotTypeMetadata) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BuiltinSlotTypeMetadata where
  parseJSON =
    Core.withObject
      "BuiltinSlotTypeMetadata"
      ( \x ->
          BuiltinSlotTypeMetadata'
            Core.<$> (x Core..:? "signature")
            Core.<*> (x Core..:? "supportedLocales" Core..!= Core.mempty)
      )

instance Core.Hashable BuiltinSlotTypeMetadata

instance Core.NFData BuiltinSlotTypeMetadata

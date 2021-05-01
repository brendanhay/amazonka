{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Locale
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a built in slot type.
--
-- /See:/ 'newBuiltinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { -- | A unique identifier for the built-in slot type. To find the signature
    -- for a slot type, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
    -- in the /Alexa Skills Kit/.
    signature :: Prelude.Maybe Prelude.Text,
    -- | A list of target locales for the slot.
    supportedLocales :: Prelude.Maybe [Locale]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { signature =
        Prelude.Nothing,
      supportedLocales = Prelude.Nothing
    }

-- | A unique identifier for the built-in slot type. To find the signature
-- for a slot type, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
builtinSlotTypeMetadata_signature :: Lens.Lens' BuiltinSlotTypeMetadata (Prelude.Maybe Prelude.Text)
builtinSlotTypeMetadata_signature = Lens.lens (\BuiltinSlotTypeMetadata' {signature} -> signature) (\s@BuiltinSlotTypeMetadata' {} a -> s {signature = a} :: BuiltinSlotTypeMetadata)

-- | A list of target locales for the slot.
builtinSlotTypeMetadata_supportedLocales :: Lens.Lens' BuiltinSlotTypeMetadata (Prelude.Maybe [Locale])
builtinSlotTypeMetadata_supportedLocales = Lens.lens (\BuiltinSlotTypeMetadata' {supportedLocales} -> supportedLocales) (\s@BuiltinSlotTypeMetadata' {} a -> s {supportedLocales = a} :: BuiltinSlotTypeMetadata) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BuiltinSlotTypeMetadata where
  parseJSON =
    Prelude.withObject
      "BuiltinSlotTypeMetadata"
      ( \x ->
          BuiltinSlotTypeMetadata'
            Prelude.<$> (x Prelude..:? "signature")
            Prelude.<*> ( x Prelude..:? "supportedLocales"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BuiltinSlotTypeMetadata

instance Prelude.NFData BuiltinSlotTypeMetadata

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
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinIntentMetadata where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Locale
import qualified Network.AWS.Prelude as Prelude

-- | Provides metadata for a built-in intent.
--
-- /See:/ 'newBuiltinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { -- | A unique identifier for the built-in intent. To find the signature for
    -- an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signature :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers for the locales that the intent supports.
    supportedLocales :: Prelude.Maybe [Locale]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BuiltinIntentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'builtinIntentMetadata_signature' - A unique identifier for the built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
--
-- 'supportedLocales', 'builtinIntentMetadata_supportedLocales' - A list of identifiers for the locales that the intent supports.
newBuiltinIntentMetadata ::
  BuiltinIntentMetadata
newBuiltinIntentMetadata =
  BuiltinIntentMetadata'
    { signature = Prelude.Nothing,
      supportedLocales = Prelude.Nothing
    }

-- | A unique identifier for the built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
builtinIntentMetadata_signature :: Lens.Lens' BuiltinIntentMetadata (Prelude.Maybe Prelude.Text)
builtinIntentMetadata_signature = Lens.lens (\BuiltinIntentMetadata' {signature} -> signature) (\s@BuiltinIntentMetadata' {} a -> s {signature = a} :: BuiltinIntentMetadata)

-- | A list of identifiers for the locales that the intent supports.
builtinIntentMetadata_supportedLocales :: Lens.Lens' BuiltinIntentMetadata (Prelude.Maybe [Locale])
builtinIntentMetadata_supportedLocales = Lens.lens (\BuiltinIntentMetadata' {supportedLocales} -> supportedLocales) (\s@BuiltinIntentMetadata' {} a -> s {supportedLocales = a} :: BuiltinIntentMetadata) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BuiltinIntentMetadata where
  parseJSON =
    Prelude.withObject
      "BuiltinIntentMetadata"
      ( \x ->
          BuiltinIntentMetadata'
            Prelude.<$> (x Prelude..:? "signature")
            Prelude.<*> ( x Prelude..:? "supportedLocales"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BuiltinIntentMetadata

instance Prelude.NFData BuiltinIntentMetadata

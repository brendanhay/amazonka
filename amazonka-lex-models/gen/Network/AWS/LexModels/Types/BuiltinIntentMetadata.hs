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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Locale

-- | Provides metadata for a built-in intent.
--
-- /See:/ 'newBuiltinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { -- | A unique identifier for the built-in intent. To find the signature for
    -- an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signature :: Core.Maybe Core.Text,
    -- | A list of identifiers for the locales that the intent supports.
    supportedLocales :: Core.Maybe [Locale]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { signature = Core.Nothing,
      supportedLocales = Core.Nothing
    }

-- | A unique identifier for the built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
builtinIntentMetadata_signature :: Lens.Lens' BuiltinIntentMetadata (Core.Maybe Core.Text)
builtinIntentMetadata_signature = Lens.lens (\BuiltinIntentMetadata' {signature} -> signature) (\s@BuiltinIntentMetadata' {} a -> s {signature = a} :: BuiltinIntentMetadata)

-- | A list of identifiers for the locales that the intent supports.
builtinIntentMetadata_supportedLocales :: Lens.Lens' BuiltinIntentMetadata (Core.Maybe [Locale])
builtinIntentMetadata_supportedLocales = Lens.lens (\BuiltinIntentMetadata' {supportedLocales} -> supportedLocales) (\s@BuiltinIntentMetadata' {} a -> s {supportedLocales = a} :: BuiltinIntentMetadata) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BuiltinIntentMetadata where
  parseJSON =
    Core.withObject
      "BuiltinIntentMetadata"
      ( \x ->
          BuiltinIntentMetadata'
            Core.<$> (x Core..:? "signature")
            Core.<*> (x Core..:? "supportedLocales" Core..!= Core.mempty)
      )

instance Core.Hashable BuiltinIntentMetadata

instance Core.NFData BuiltinIntentMetadata

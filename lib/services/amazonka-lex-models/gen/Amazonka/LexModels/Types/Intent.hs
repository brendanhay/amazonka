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
-- Module      : Amazonka.LexModels.Types.Intent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.Intent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the specific version of an intent.
--
-- /See:/ 'newIntent' smart constructor.
data Intent = Intent'
  { -- | The name of the intent.
    intentName :: Prelude.Text,
    -- | The version of the intent.
    intentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Intent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'intent_intentName' - The name of the intent.
--
-- 'intentVersion', 'intent_intentVersion' - The version of the intent.
newIntent ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'intentVersion'
  Prelude.Text ->
  Intent
newIntent pIntentName_ pIntentVersion_ =
  Intent'
    { intentName = pIntentName_,
      intentVersion = pIntentVersion_
    }

-- | The name of the intent.
intent_intentName :: Lens.Lens' Intent Prelude.Text
intent_intentName = Lens.lens (\Intent' {intentName} -> intentName) (\s@Intent' {} a -> s {intentName = a} :: Intent)

-- | The version of the intent.
intent_intentVersion :: Lens.Lens' Intent Prelude.Text
intent_intentVersion = Lens.lens (\Intent' {intentVersion} -> intentVersion) (\s@Intent' {} a -> s {intentVersion = a} :: Intent)

instance Data.FromJSON Intent where
  parseJSON =
    Data.withObject
      "Intent"
      ( \x ->
          Intent'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "intentVersion")
      )

instance Prelude.Hashable Intent where
  hashWithSalt _salt Intent' {..} =
    _salt
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` intentVersion

instance Prelude.NFData Intent where
  rnf Intent' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf intentVersion

instance Data.ToJSON Intent where
  toJSON Intent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("intentName" Data..= intentName),
            Prelude.Just
              ("intentVersion" Data..= intentVersion)
          ]
      )

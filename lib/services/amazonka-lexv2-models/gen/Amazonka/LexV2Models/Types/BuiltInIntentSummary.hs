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
-- Module      : Amazonka.LexV2Models.Types.BuiltInIntentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BuiltInIntentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about a built-in intent for the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListBuiltInIntents.html ListBuiltInIntents>
-- operation.
--
-- /See:/ 'newBuiltInIntentSummary' smart constructor.
data BuiltInIntentSummary = BuiltInIntentSummary'
  { -- | The signature of the built-in intent. Use this to specify the parent
    -- intent of a derived intent.
    intentSignature :: Prelude.Maybe Prelude.Text,
    -- | The description of the intent.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuiltInIntentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentSignature', 'builtInIntentSummary_intentSignature' - The signature of the built-in intent. Use this to specify the parent
-- intent of a derived intent.
--
-- 'description', 'builtInIntentSummary_description' - The description of the intent.
newBuiltInIntentSummary ::
  BuiltInIntentSummary
newBuiltInIntentSummary =
  BuiltInIntentSummary'
    { intentSignature =
        Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The signature of the built-in intent. Use this to specify the parent
-- intent of a derived intent.
builtInIntentSummary_intentSignature :: Lens.Lens' BuiltInIntentSummary (Prelude.Maybe Prelude.Text)
builtInIntentSummary_intentSignature = Lens.lens (\BuiltInIntentSummary' {intentSignature} -> intentSignature) (\s@BuiltInIntentSummary' {} a -> s {intentSignature = a} :: BuiltInIntentSummary)

-- | The description of the intent.
builtInIntentSummary_description :: Lens.Lens' BuiltInIntentSummary (Prelude.Maybe Prelude.Text)
builtInIntentSummary_description = Lens.lens (\BuiltInIntentSummary' {description} -> description) (\s@BuiltInIntentSummary' {} a -> s {description = a} :: BuiltInIntentSummary)

instance Core.FromJSON BuiltInIntentSummary where
  parseJSON =
    Core.withObject
      "BuiltInIntentSummary"
      ( \x ->
          BuiltInIntentSummary'
            Prelude.<$> (x Core..:? "intentSignature")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable BuiltInIntentSummary where
  hashWithSalt _salt BuiltInIntentSummary' {..} =
    _salt `Prelude.hashWithSalt` intentSignature
      `Prelude.hashWithSalt` description

instance Prelude.NFData BuiltInIntentSummary where
  rnf BuiltInIntentSummary' {..} =
    Prelude.rnf intentSignature
      `Prelude.seq` Prelude.rnf description

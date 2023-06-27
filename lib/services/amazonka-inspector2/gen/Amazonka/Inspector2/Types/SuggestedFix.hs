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
-- Module      : Amazonka.Inspector2.Types.SuggestedFix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.SuggestedFix where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A suggested fix for a vulnerability in your Lambda function code.
--
-- /See:/ 'newSuggestedFix' smart constructor.
data SuggestedFix = SuggestedFix'
  { -- | The fix\'s code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The fix\'s description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestedFix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'suggestedFix_code' - The fix\'s code.
--
-- 'description', 'suggestedFix_description' - The fix\'s description.
newSuggestedFix ::
  SuggestedFix
newSuggestedFix =
  SuggestedFix'
    { code = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The fix\'s code.
suggestedFix_code :: Lens.Lens' SuggestedFix (Prelude.Maybe Prelude.Text)
suggestedFix_code = Lens.lens (\SuggestedFix' {code} -> code) (\s@SuggestedFix' {} a -> s {code = a} :: SuggestedFix)

-- | The fix\'s description.
suggestedFix_description :: Lens.Lens' SuggestedFix (Prelude.Maybe Prelude.Text)
suggestedFix_description = Lens.lens (\SuggestedFix' {description} -> description) (\s@SuggestedFix' {} a -> s {description = a} :: SuggestedFix)

instance Data.FromJSON SuggestedFix where
  parseJSON =
    Data.withObject
      "SuggestedFix"
      ( \x ->
          SuggestedFix'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable SuggestedFix where
  hashWithSalt _salt SuggestedFix' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` description

instance Prelude.NFData SuggestedFix where
  rnf SuggestedFix' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf description

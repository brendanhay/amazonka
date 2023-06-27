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
-- Module      : Amazonka.WellArchitected.Types.ChoiceContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The choice content.
--
-- /See:/ 'newChoiceContent' smart constructor.
data ChoiceContent = ChoiceContent'
  { -- | The display text for the choice content.
    displayText :: Prelude.Maybe Prelude.Text,
    -- | The URL for the choice content.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChoiceContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayText', 'choiceContent_displayText' - The display text for the choice content.
--
-- 'url', 'choiceContent_url' - The URL for the choice content.
newChoiceContent ::
  ChoiceContent
newChoiceContent =
  ChoiceContent'
    { displayText = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The display text for the choice content.
choiceContent_displayText :: Lens.Lens' ChoiceContent (Prelude.Maybe Prelude.Text)
choiceContent_displayText = Lens.lens (\ChoiceContent' {displayText} -> displayText) (\s@ChoiceContent' {} a -> s {displayText = a} :: ChoiceContent)

-- | The URL for the choice content.
choiceContent_url :: Lens.Lens' ChoiceContent (Prelude.Maybe Prelude.Text)
choiceContent_url = Lens.lens (\ChoiceContent' {url} -> url) (\s@ChoiceContent' {} a -> s {url = a} :: ChoiceContent)

instance Data.FromJSON ChoiceContent where
  parseJSON =
    Data.withObject
      "ChoiceContent"
      ( \x ->
          ChoiceContent'
            Prelude.<$> (x Data..:? "DisplayText")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable ChoiceContent where
  hashWithSalt _salt ChoiceContent' {..} =
    _salt
      `Prelude.hashWithSalt` displayText
      `Prelude.hashWithSalt` url

instance Prelude.NFData ChoiceContent where
  rnf ChoiceContent' {..} =
    Prelude.rnf displayText
      `Prelude.seq` Prelude.rnf url

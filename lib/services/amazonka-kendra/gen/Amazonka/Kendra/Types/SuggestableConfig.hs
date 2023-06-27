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
-- Module      : Amazonka.Kendra.Types.SuggestableConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SuggestableConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for a document field\/attribute
-- that you want to base query suggestions on.
--
-- /See:/ 'newSuggestableConfig' smart constructor.
data SuggestableConfig = SuggestableConfig'
  { -- | The name of the document field\/attribute.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | @TRUE@ means the document field\/attribute is suggestible, so the
    -- contents within the field can be used for query suggestions.
    suggestable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestableConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'suggestableConfig_attributeName' - The name of the document field\/attribute.
--
-- 'suggestable', 'suggestableConfig_suggestable' - @TRUE@ means the document field\/attribute is suggestible, so the
-- contents within the field can be used for query suggestions.
newSuggestableConfig ::
  SuggestableConfig
newSuggestableConfig =
  SuggestableConfig'
    { attributeName = Prelude.Nothing,
      suggestable = Prelude.Nothing
    }

-- | The name of the document field\/attribute.
suggestableConfig_attributeName :: Lens.Lens' SuggestableConfig (Prelude.Maybe Prelude.Text)
suggestableConfig_attributeName = Lens.lens (\SuggestableConfig' {attributeName} -> attributeName) (\s@SuggestableConfig' {} a -> s {attributeName = a} :: SuggestableConfig)

-- | @TRUE@ means the document field\/attribute is suggestible, so the
-- contents within the field can be used for query suggestions.
suggestableConfig_suggestable :: Lens.Lens' SuggestableConfig (Prelude.Maybe Prelude.Bool)
suggestableConfig_suggestable = Lens.lens (\SuggestableConfig' {suggestable} -> suggestable) (\s@SuggestableConfig' {} a -> s {suggestable = a} :: SuggestableConfig)

instance Data.FromJSON SuggestableConfig where
  parseJSON =
    Data.withObject
      "SuggestableConfig"
      ( \x ->
          SuggestableConfig'
            Prelude.<$> (x Data..:? "AttributeName")
            Prelude.<*> (x Data..:? "Suggestable")
      )

instance Prelude.Hashable SuggestableConfig where
  hashWithSalt _salt SuggestableConfig' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` suggestable

instance Prelude.NFData SuggestableConfig where
  rnf SuggestableConfig' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf suggestable

instance Data.ToJSON SuggestableConfig where
  toJSON SuggestableConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeName" Data..=) Prelude.<$> attributeName,
            ("Suggestable" Data..=) Prelude.<$> suggestable
          ]
      )

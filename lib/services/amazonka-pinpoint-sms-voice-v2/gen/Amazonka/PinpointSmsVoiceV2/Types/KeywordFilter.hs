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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.KeywordFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.KeywordFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.KeywordFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for keywords that meet a specified criteria.
--
-- /See:/ 'newKeywordFilter' smart constructor.
data KeywordFilter = KeywordFilter'
  { -- | The name of the attribute to filter on.
    name :: KeywordFilterName,
    -- | An array values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeywordFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'keywordFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'keywordFilter_values' - An array values to filter for.
newKeywordFilter ::
  -- | 'name'
  KeywordFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  KeywordFilter
newKeywordFilter pName_ pValues_ =
  KeywordFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
keywordFilter_name :: Lens.Lens' KeywordFilter KeywordFilterName
keywordFilter_name = Lens.lens (\KeywordFilter' {name} -> name) (\s@KeywordFilter' {} a -> s {name = a} :: KeywordFilter)

-- | An array values to filter for.
keywordFilter_values :: Lens.Lens' KeywordFilter (Prelude.NonEmpty Prelude.Text)
keywordFilter_values = Lens.lens (\KeywordFilter' {values} -> values) (\s@KeywordFilter' {} a -> s {values = a} :: KeywordFilter) Prelude.. Lens.coerced

instance Prelude.Hashable KeywordFilter where
  hashWithSalt _salt KeywordFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData KeywordFilter where
  rnf KeywordFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON KeywordFilter where
  toJSON KeywordFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )

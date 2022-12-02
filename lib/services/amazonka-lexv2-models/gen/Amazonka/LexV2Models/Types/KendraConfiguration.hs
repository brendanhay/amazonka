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
-- Module      : Amazonka.LexV2Models.Types.KendraConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.KendraConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration information for the AMAZON.KendraSearchIntent
-- intent. When you use this intent, Amazon Lex searches the specified
-- Amazon Kendra index and returns documents from the index that match the
-- user\'s utterance.
--
-- /See:/ 'newKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { -- | Determines whether the AMAZON.KendraSearchIntent intent uses a custom
    -- query string to query the Amazon Kendra index.
    queryFilterStringEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
    -- response from a query. The filter is in the format defined by Amazon
    -- Kendra. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
    queryFilterString :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
    -- the AMAZON.KendraSearchIntent intent to search. The index must be in the
    -- same account and Region as the Amazon Lex bot.
    kendraIndex :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KendraConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryFilterStringEnabled', 'kendraConfiguration_queryFilterStringEnabled' - Determines whether the AMAZON.KendraSearchIntent intent uses a custom
-- query string to query the Amazon Kendra index.
--
-- 'queryFilterString', 'kendraConfiguration_queryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from a query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- 'kendraIndex', 'kendraConfiguration_kendraIndex' - The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot.
newKendraConfiguration ::
  -- | 'kendraIndex'
  Prelude.Text ->
  KendraConfiguration
newKendraConfiguration pKendraIndex_ =
  KendraConfiguration'
    { queryFilterStringEnabled =
        Prelude.Nothing,
      queryFilterString = Prelude.Nothing,
      kendraIndex = pKendraIndex_
    }

-- | Determines whether the AMAZON.KendraSearchIntent intent uses a custom
-- query string to query the Amazon Kendra index.
kendraConfiguration_queryFilterStringEnabled :: Lens.Lens' KendraConfiguration (Prelude.Maybe Prelude.Bool)
kendraConfiguration_queryFilterStringEnabled = Lens.lens (\KendraConfiguration' {queryFilterStringEnabled} -> queryFilterStringEnabled) (\s@KendraConfiguration' {} a -> s {queryFilterStringEnabled = a} :: KendraConfiguration)

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from a query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
kendraConfiguration_queryFilterString :: Lens.Lens' KendraConfiguration (Prelude.Maybe Prelude.Text)
kendraConfiguration_queryFilterString = Lens.lens (\KendraConfiguration' {queryFilterString} -> queryFilterString) (\s@KendraConfiguration' {} a -> s {queryFilterString = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot.
kendraConfiguration_kendraIndex :: Lens.Lens' KendraConfiguration Prelude.Text
kendraConfiguration_kendraIndex = Lens.lens (\KendraConfiguration' {kendraIndex} -> kendraIndex) (\s@KendraConfiguration' {} a -> s {kendraIndex = a} :: KendraConfiguration)

instance Data.FromJSON KendraConfiguration where
  parseJSON =
    Data.withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            Prelude.<$> (x Data..:? "queryFilterStringEnabled")
            Prelude.<*> (x Data..:? "queryFilterString")
            Prelude.<*> (x Data..: "kendraIndex")
      )

instance Prelude.Hashable KendraConfiguration where
  hashWithSalt _salt KendraConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` queryFilterStringEnabled
      `Prelude.hashWithSalt` queryFilterString
      `Prelude.hashWithSalt` kendraIndex

instance Prelude.NFData KendraConfiguration where
  rnf KendraConfiguration' {..} =
    Prelude.rnf queryFilterStringEnabled
      `Prelude.seq` Prelude.rnf queryFilterString
      `Prelude.seq` Prelude.rnf kendraIndex

instance Data.ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("queryFilterStringEnabled" Data..=)
              Prelude.<$> queryFilterStringEnabled,
            ("queryFilterString" Data..=)
              Prelude.<$> queryFilterString,
            Prelude.Just ("kendraIndex" Data..= kendraIndex)
          ]
      )

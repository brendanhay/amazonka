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
-- Module      : Network.AWS.LexV2Models.Types.KendraConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.KendraConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for the AMAZON.KendraSearchIntent
-- intent. When you use this intent, Amazon Lex searches the specified
-- Amazon Kendra index and returns documents from the index that match the
-- user\'s utterance.
--
-- /See:/ 'newKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { -- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
    -- response from a query. The filter is in the format defined by Amazon
    -- Kendra. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
    queryFilterString :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the AMAZON.KendraSearchIntent intent uses a custom
    -- query string to query the Amazon Kendra index.
    queryFilterStringEnabled :: Prelude.Maybe Prelude.Bool,
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
-- 'queryFilterString', 'kendraConfiguration_queryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from a query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- 'queryFilterStringEnabled', 'kendraConfiguration_queryFilterStringEnabled' - Determines whether the AMAZON.KendraSearchIntent intent uses a custom
-- query string to query the Amazon Kendra index.
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
    { queryFilterString =
        Prelude.Nothing,
      queryFilterStringEnabled = Prelude.Nothing,
      kendraIndex = pKendraIndex_
    }

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from a query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
kendraConfiguration_queryFilterString :: Lens.Lens' KendraConfiguration (Prelude.Maybe Prelude.Text)
kendraConfiguration_queryFilterString = Lens.lens (\KendraConfiguration' {queryFilterString} -> queryFilterString) (\s@KendraConfiguration' {} a -> s {queryFilterString = a} :: KendraConfiguration)

-- | Determines whether the AMAZON.KendraSearchIntent intent uses a custom
-- query string to query the Amazon Kendra index.
kendraConfiguration_queryFilterStringEnabled :: Lens.Lens' KendraConfiguration (Prelude.Maybe Prelude.Bool)
kendraConfiguration_queryFilterStringEnabled = Lens.lens (\KendraConfiguration' {queryFilterStringEnabled} -> queryFilterStringEnabled) (\s@KendraConfiguration' {} a -> s {queryFilterStringEnabled = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot.
kendraConfiguration_kendraIndex :: Lens.Lens' KendraConfiguration Prelude.Text
kendraConfiguration_kendraIndex = Lens.lens (\KendraConfiguration' {kendraIndex} -> kendraIndex) (\s@KendraConfiguration' {} a -> s {kendraIndex = a} :: KendraConfiguration)

instance Core.FromJSON KendraConfiguration where
  parseJSON =
    Core.withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            Prelude.<$> (x Core..:? "queryFilterString")
            Prelude.<*> (x Core..:? "queryFilterStringEnabled")
            Prelude.<*> (x Core..: "kendraIndex")
      )

instance Prelude.Hashable KendraConfiguration

instance Prelude.NFData KendraConfiguration

instance Core.ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("queryFilterString" Core..=)
              Prelude.<$> queryFilterString,
            ("queryFilterStringEnabled" Core..=)
              Prelude.<$> queryFilterStringEnabled,
            Prelude.Just ("kendraIndex" Core..= kendraIndex)
          ]
      )

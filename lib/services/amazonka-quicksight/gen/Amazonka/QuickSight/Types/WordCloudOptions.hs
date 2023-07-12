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
-- Module      : Amazonka.QuickSight.Types.WordCloudOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.WordCloudCloudLayout
import Amazonka.QuickSight.Types.WordCloudWordCasing
import Amazonka.QuickSight.Types.WordCloudWordOrientation
import Amazonka.QuickSight.Types.WordCloudWordPadding
import Amazonka.QuickSight.Types.WordCloudWordScaling

-- | The word cloud options for a word cloud visual.
--
-- /See:/ 'newWordCloudOptions' smart constructor.
data WordCloudOptions = WordCloudOptions'
  { -- | The cloud layout options (fluid, normal) of a word cloud.
    cloudLayout :: Prelude.Maybe WordCloudCloudLayout,
    -- | The length limit of each word from 1-100.
    maximumStringLength :: Prelude.Maybe Prelude.Natural,
    -- | The word casing options (lower_case, existing_case) for the words in a
    -- word cloud.
    wordCasing :: Prelude.Maybe WordCloudWordCasing,
    -- | The word orientation options (horizontal, horizontal_and_vertical) for
    -- the words in a word cloud.
    wordOrientation :: Prelude.Maybe WordCloudWordOrientation,
    -- | The word padding options (none, small, medium, large) for the words in a
    -- word cloud.
    wordPadding :: Prelude.Maybe WordCloudWordPadding,
    -- | The word scaling options (emphasize, normal) for the words in a word
    -- cloud.
    wordScaling :: Prelude.Maybe WordCloudWordScaling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WordCloudOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudLayout', 'wordCloudOptions_cloudLayout' - The cloud layout options (fluid, normal) of a word cloud.
--
-- 'maximumStringLength', 'wordCloudOptions_maximumStringLength' - The length limit of each word from 1-100.
--
-- 'wordCasing', 'wordCloudOptions_wordCasing' - The word casing options (lower_case, existing_case) for the words in a
-- word cloud.
--
-- 'wordOrientation', 'wordCloudOptions_wordOrientation' - The word orientation options (horizontal, horizontal_and_vertical) for
-- the words in a word cloud.
--
-- 'wordPadding', 'wordCloudOptions_wordPadding' - The word padding options (none, small, medium, large) for the words in a
-- word cloud.
--
-- 'wordScaling', 'wordCloudOptions_wordScaling' - The word scaling options (emphasize, normal) for the words in a word
-- cloud.
newWordCloudOptions ::
  WordCloudOptions
newWordCloudOptions =
  WordCloudOptions'
    { cloudLayout = Prelude.Nothing,
      maximumStringLength = Prelude.Nothing,
      wordCasing = Prelude.Nothing,
      wordOrientation = Prelude.Nothing,
      wordPadding = Prelude.Nothing,
      wordScaling = Prelude.Nothing
    }

-- | The cloud layout options (fluid, normal) of a word cloud.
wordCloudOptions_cloudLayout :: Lens.Lens' WordCloudOptions (Prelude.Maybe WordCloudCloudLayout)
wordCloudOptions_cloudLayout = Lens.lens (\WordCloudOptions' {cloudLayout} -> cloudLayout) (\s@WordCloudOptions' {} a -> s {cloudLayout = a} :: WordCloudOptions)

-- | The length limit of each word from 1-100.
wordCloudOptions_maximumStringLength :: Lens.Lens' WordCloudOptions (Prelude.Maybe Prelude.Natural)
wordCloudOptions_maximumStringLength = Lens.lens (\WordCloudOptions' {maximumStringLength} -> maximumStringLength) (\s@WordCloudOptions' {} a -> s {maximumStringLength = a} :: WordCloudOptions)

-- | The word casing options (lower_case, existing_case) for the words in a
-- word cloud.
wordCloudOptions_wordCasing :: Lens.Lens' WordCloudOptions (Prelude.Maybe WordCloudWordCasing)
wordCloudOptions_wordCasing = Lens.lens (\WordCloudOptions' {wordCasing} -> wordCasing) (\s@WordCloudOptions' {} a -> s {wordCasing = a} :: WordCloudOptions)

-- | The word orientation options (horizontal, horizontal_and_vertical) for
-- the words in a word cloud.
wordCloudOptions_wordOrientation :: Lens.Lens' WordCloudOptions (Prelude.Maybe WordCloudWordOrientation)
wordCloudOptions_wordOrientation = Lens.lens (\WordCloudOptions' {wordOrientation} -> wordOrientation) (\s@WordCloudOptions' {} a -> s {wordOrientation = a} :: WordCloudOptions)

-- | The word padding options (none, small, medium, large) for the words in a
-- word cloud.
wordCloudOptions_wordPadding :: Lens.Lens' WordCloudOptions (Prelude.Maybe WordCloudWordPadding)
wordCloudOptions_wordPadding = Lens.lens (\WordCloudOptions' {wordPadding} -> wordPadding) (\s@WordCloudOptions' {} a -> s {wordPadding = a} :: WordCloudOptions)

-- | The word scaling options (emphasize, normal) for the words in a word
-- cloud.
wordCloudOptions_wordScaling :: Lens.Lens' WordCloudOptions (Prelude.Maybe WordCloudWordScaling)
wordCloudOptions_wordScaling = Lens.lens (\WordCloudOptions' {wordScaling} -> wordScaling) (\s@WordCloudOptions' {} a -> s {wordScaling = a} :: WordCloudOptions)

instance Data.FromJSON WordCloudOptions where
  parseJSON =
    Data.withObject
      "WordCloudOptions"
      ( \x ->
          WordCloudOptions'
            Prelude.<$> (x Data..:? "CloudLayout")
            Prelude.<*> (x Data..:? "MaximumStringLength")
            Prelude.<*> (x Data..:? "WordCasing")
            Prelude.<*> (x Data..:? "WordOrientation")
            Prelude.<*> (x Data..:? "WordPadding")
            Prelude.<*> (x Data..:? "WordScaling")
      )

instance Prelude.Hashable WordCloudOptions where
  hashWithSalt _salt WordCloudOptions' {..} =
    _salt
      `Prelude.hashWithSalt` cloudLayout
      `Prelude.hashWithSalt` maximumStringLength
      `Prelude.hashWithSalt` wordCasing
      `Prelude.hashWithSalt` wordOrientation
      `Prelude.hashWithSalt` wordPadding
      `Prelude.hashWithSalt` wordScaling

instance Prelude.NFData WordCloudOptions where
  rnf WordCloudOptions' {..} =
    Prelude.rnf cloudLayout
      `Prelude.seq` Prelude.rnf maximumStringLength
      `Prelude.seq` Prelude.rnf wordCasing
      `Prelude.seq` Prelude.rnf wordOrientation
      `Prelude.seq` Prelude.rnf wordPadding
      `Prelude.seq` Prelude.rnf wordScaling

instance Data.ToJSON WordCloudOptions where
  toJSON WordCloudOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudLayout" Data..=) Prelude.<$> cloudLayout,
            ("MaximumStringLength" Data..=)
              Prelude.<$> maximumStringLength,
            ("WordCasing" Data..=) Prelude.<$> wordCasing,
            ("WordOrientation" Data..=)
              Prelude.<$> wordOrientation,
            ("WordPadding" Data..=) Prelude.<$> wordPadding,
            ("WordScaling" Data..=) Prelude.<$> wordScaling
          ]
      )

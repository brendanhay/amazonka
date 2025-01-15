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
-- Module      : Amazonka.Kendra.Types.Relevance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Relevance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.Order
import qualified Amazonka.Prelude as Prelude

-- | Provides information for tuning the relevance of a field in a search.
-- When a query includes terms that match the field, the results are given
-- a boost in the response based on these tuning parameters.
--
-- /See:/ 'newRelevance' smart constructor.
data Relevance = Relevance'
  { -- | Specifies the time period that the boost applies to. For example, to
    -- make the boost apply to documents with the field value within the last
    -- month, you would use \"2628000s\". Once the field value is beyond the
    -- specified range, the effect of the boost drops off. The higher the
    -- importance, the faster the effect drops off. If you don\'t specify a
    -- value, the default is 3 months. The value of the field is a numeric
    -- string followed by the character \"s\", for example \"86400s\" for one
    -- day, or \"604800s\" for one week.
    --
    -- Only applies to @DATE@ fields.
    duration :: Prelude.Maybe Prelude.Text,
    -- | Indicates that this field determines how \"fresh\" a document is. For
    -- example, if document 1 was created on November 5, and document 2 was
    -- created on October 31, document 1 is \"fresher\" than document 2. You
    -- can only set the @Freshness@ field on one @DATE@ type field. Only
    -- applies to @DATE@ fields.
    freshness :: Prelude.Maybe Prelude.Bool,
    -- | The relative importance of the field in the search. Larger numbers
    -- provide more of a boost than smaller numbers.
    importance :: Prelude.Maybe Prelude.Natural,
    -- | Determines how values should be interpreted.
    --
    -- When the @RankOrder@ field is @ASCENDING@, higher numbers are better.
    -- For example, a document with a rating score of 10 is higher ranking than
    -- a document with a rating score of 1.
    --
    -- When the @RankOrder@ field is @DESCENDING@, lower numbers are better.
    -- For example, in a task tracking application, a priority 1 task is more
    -- important than a priority 5 task.
    --
    -- Only applies to @LONG@ and @DOUBLE@ fields.
    rankOrder :: Prelude.Maybe Order,
    -- | A list of values that should be given a different boost when they appear
    -- in the result list. For example, if you are boosting a field called
    -- \"department,\" query terms that match the department field are boosted
    -- in the result. However, you can add entries from the department field to
    -- boost documents with those values higher.
    --
    -- For example, you can add entries to the map with names of departments.
    -- If you add \"HR\",5 and \"Legal\",3 those departments are given special
    -- attention when they appear in the metadata of a document. When those
    -- terms appear they are given the specified importance instead of the
    -- regular importance for the boost.
    valueImportanceMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Relevance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'relevance_duration' - Specifies the time period that the boost applies to. For example, to
-- make the boost apply to documents with the field value within the last
-- month, you would use \"2628000s\". Once the field value is beyond the
-- specified range, the effect of the boost drops off. The higher the
-- importance, the faster the effect drops off. If you don\'t specify a
-- value, the default is 3 months. The value of the field is a numeric
-- string followed by the character \"s\", for example \"86400s\" for one
-- day, or \"604800s\" for one week.
--
-- Only applies to @DATE@ fields.
--
-- 'freshness', 'relevance_freshness' - Indicates that this field determines how \"fresh\" a document is. For
-- example, if document 1 was created on November 5, and document 2 was
-- created on October 31, document 1 is \"fresher\" than document 2. You
-- can only set the @Freshness@ field on one @DATE@ type field. Only
-- applies to @DATE@ fields.
--
-- 'importance', 'relevance_importance' - The relative importance of the field in the search. Larger numbers
-- provide more of a boost than smaller numbers.
--
-- 'rankOrder', 'relevance_rankOrder' - Determines how values should be interpreted.
--
-- When the @RankOrder@ field is @ASCENDING@, higher numbers are better.
-- For example, a document with a rating score of 10 is higher ranking than
-- a document with a rating score of 1.
--
-- When the @RankOrder@ field is @DESCENDING@, lower numbers are better.
-- For example, in a task tracking application, a priority 1 task is more
-- important than a priority 5 task.
--
-- Only applies to @LONG@ and @DOUBLE@ fields.
--
-- 'valueImportanceMap', 'relevance_valueImportanceMap' - A list of values that should be given a different boost when they appear
-- in the result list. For example, if you are boosting a field called
-- \"department,\" query terms that match the department field are boosted
-- in the result. However, you can add entries from the department field to
-- boost documents with those values higher.
--
-- For example, you can add entries to the map with names of departments.
-- If you add \"HR\",5 and \"Legal\",3 those departments are given special
-- attention when they appear in the metadata of a document. When those
-- terms appear they are given the specified importance instead of the
-- regular importance for the boost.
newRelevance ::
  Relevance
newRelevance =
  Relevance'
    { duration = Prelude.Nothing,
      freshness = Prelude.Nothing,
      importance = Prelude.Nothing,
      rankOrder = Prelude.Nothing,
      valueImportanceMap = Prelude.Nothing
    }

-- | Specifies the time period that the boost applies to. For example, to
-- make the boost apply to documents with the field value within the last
-- month, you would use \"2628000s\". Once the field value is beyond the
-- specified range, the effect of the boost drops off. The higher the
-- importance, the faster the effect drops off. If you don\'t specify a
-- value, the default is 3 months. The value of the field is a numeric
-- string followed by the character \"s\", for example \"86400s\" for one
-- day, or \"604800s\" for one week.
--
-- Only applies to @DATE@ fields.
relevance_duration :: Lens.Lens' Relevance (Prelude.Maybe Prelude.Text)
relevance_duration = Lens.lens (\Relevance' {duration} -> duration) (\s@Relevance' {} a -> s {duration = a} :: Relevance)

-- | Indicates that this field determines how \"fresh\" a document is. For
-- example, if document 1 was created on November 5, and document 2 was
-- created on October 31, document 1 is \"fresher\" than document 2. You
-- can only set the @Freshness@ field on one @DATE@ type field. Only
-- applies to @DATE@ fields.
relevance_freshness :: Lens.Lens' Relevance (Prelude.Maybe Prelude.Bool)
relevance_freshness = Lens.lens (\Relevance' {freshness} -> freshness) (\s@Relevance' {} a -> s {freshness = a} :: Relevance)

-- | The relative importance of the field in the search. Larger numbers
-- provide more of a boost than smaller numbers.
relevance_importance :: Lens.Lens' Relevance (Prelude.Maybe Prelude.Natural)
relevance_importance = Lens.lens (\Relevance' {importance} -> importance) (\s@Relevance' {} a -> s {importance = a} :: Relevance)

-- | Determines how values should be interpreted.
--
-- When the @RankOrder@ field is @ASCENDING@, higher numbers are better.
-- For example, a document with a rating score of 10 is higher ranking than
-- a document with a rating score of 1.
--
-- When the @RankOrder@ field is @DESCENDING@, lower numbers are better.
-- For example, in a task tracking application, a priority 1 task is more
-- important than a priority 5 task.
--
-- Only applies to @LONG@ and @DOUBLE@ fields.
relevance_rankOrder :: Lens.Lens' Relevance (Prelude.Maybe Order)
relevance_rankOrder = Lens.lens (\Relevance' {rankOrder} -> rankOrder) (\s@Relevance' {} a -> s {rankOrder = a} :: Relevance)

-- | A list of values that should be given a different boost when they appear
-- in the result list. For example, if you are boosting a field called
-- \"department,\" query terms that match the department field are boosted
-- in the result. However, you can add entries from the department field to
-- boost documents with those values higher.
--
-- For example, you can add entries to the map with names of departments.
-- If you add \"HR\",5 and \"Legal\",3 those departments are given special
-- attention when they appear in the metadata of a document. When those
-- terms appear they are given the specified importance instead of the
-- regular importance for the boost.
relevance_valueImportanceMap :: Lens.Lens' Relevance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
relevance_valueImportanceMap = Lens.lens (\Relevance' {valueImportanceMap} -> valueImportanceMap) (\s@Relevance' {} a -> s {valueImportanceMap = a} :: Relevance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Relevance where
  parseJSON =
    Data.withObject
      "Relevance"
      ( \x ->
          Relevance'
            Prelude.<$> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "Freshness")
            Prelude.<*> (x Data..:? "Importance")
            Prelude.<*> (x Data..:? "RankOrder")
            Prelude.<*> ( x
                            Data..:? "ValueImportanceMap"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Relevance where
  hashWithSalt _salt Relevance' {..} =
    _salt
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` freshness
      `Prelude.hashWithSalt` importance
      `Prelude.hashWithSalt` rankOrder
      `Prelude.hashWithSalt` valueImportanceMap

instance Prelude.NFData Relevance where
  rnf Relevance' {..} =
    Prelude.rnf duration `Prelude.seq`
      Prelude.rnf freshness `Prelude.seq`
        Prelude.rnf importance `Prelude.seq`
          Prelude.rnf rankOrder `Prelude.seq`
            Prelude.rnf valueImportanceMap

instance Data.ToJSON Relevance where
  toJSON Relevance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Duration" Data..=) Prelude.<$> duration,
            ("Freshness" Data..=) Prelude.<$> freshness,
            ("Importance" Data..=) Prelude.<$> importance,
            ("RankOrder" Data..=) Prelude.<$> rankOrder,
            ("ValueImportanceMap" Data..=)
              Prelude.<$> valueImportanceMap
          ]
      )

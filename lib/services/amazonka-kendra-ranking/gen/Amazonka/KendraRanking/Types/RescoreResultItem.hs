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
-- Module      : Amazonka.KendraRanking.Types.RescoreResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types.RescoreResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A result item for a document with a new relevancy score.
--
-- /See:/ 'newRescoreResultItem' smart constructor.
data RescoreResultItem = RescoreResultItem'
  { -- | The identifier of the document from the search service.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | The relevancy score or rank that Amazon Kendra Intelligent Ranking gives
    -- to the result.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RescoreResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentId', 'rescoreResultItem_documentId' - The identifier of the document from the search service.
--
-- 'score', 'rescoreResultItem_score' - The relevancy score or rank that Amazon Kendra Intelligent Ranking gives
-- to the result.
newRescoreResultItem ::
  RescoreResultItem
newRescoreResultItem =
  RescoreResultItem'
    { documentId = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The identifier of the document from the search service.
rescoreResultItem_documentId :: Lens.Lens' RescoreResultItem (Prelude.Maybe Prelude.Text)
rescoreResultItem_documentId = Lens.lens (\RescoreResultItem' {documentId} -> documentId) (\s@RescoreResultItem' {} a -> s {documentId = a} :: RescoreResultItem)

-- | The relevancy score or rank that Amazon Kendra Intelligent Ranking gives
-- to the result.
rescoreResultItem_score :: Lens.Lens' RescoreResultItem (Prelude.Maybe Prelude.Double)
rescoreResultItem_score = Lens.lens (\RescoreResultItem' {score} -> score) (\s@RescoreResultItem' {} a -> s {score = a} :: RescoreResultItem)

instance Data.FromJSON RescoreResultItem where
  parseJSON =
    Data.withObject
      "RescoreResultItem"
      ( \x ->
          RescoreResultItem'
            Prelude.<$> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable RescoreResultItem where
  hashWithSalt _salt RescoreResultItem' {..} =
    _salt
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` score

instance Prelude.NFData RescoreResultItem where
  rnf RescoreResultItem' {..} =
    Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf score

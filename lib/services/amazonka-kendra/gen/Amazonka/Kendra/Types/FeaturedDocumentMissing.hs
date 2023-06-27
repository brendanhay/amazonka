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
-- Module      : Amazonka.Kendra.Types.FeaturedDocumentMissing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedDocumentMissing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A document ID doesn\'t exist but you have specified as a featured
-- document. Amazon Kendra cannot feature the document if it doesn\'t exist
-- in the index. You can check the status of a document and its ID or check
-- for documents with status errors using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchGetDocumentStatus.html BatchGetDocumentStatus>
-- API.
--
-- /See:/ 'newFeaturedDocumentMissing' smart constructor.
data FeaturedDocumentMissing = FeaturedDocumentMissing'
  { -- | The identifier of the document that doesn\'t exist but you have
    -- specified as a featured document.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturedDocumentMissing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'featuredDocumentMissing_id' - The identifier of the document that doesn\'t exist but you have
-- specified as a featured document.
newFeaturedDocumentMissing ::
  FeaturedDocumentMissing
newFeaturedDocumentMissing =
  FeaturedDocumentMissing' {id = Prelude.Nothing}

-- | The identifier of the document that doesn\'t exist but you have
-- specified as a featured document.
featuredDocumentMissing_id :: Lens.Lens' FeaturedDocumentMissing (Prelude.Maybe Prelude.Text)
featuredDocumentMissing_id = Lens.lens (\FeaturedDocumentMissing' {id} -> id) (\s@FeaturedDocumentMissing' {} a -> s {id = a} :: FeaturedDocumentMissing)

instance Data.FromJSON FeaturedDocumentMissing where
  parseJSON =
    Data.withObject
      "FeaturedDocumentMissing"
      ( \x ->
          FeaturedDocumentMissing'
            Prelude.<$> (x Data..:? "Id")
      )

instance Prelude.Hashable FeaturedDocumentMissing where
  hashWithSalt _salt FeaturedDocumentMissing' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData FeaturedDocumentMissing where
  rnf FeaturedDocumentMissing' {..} = Prelude.rnf id

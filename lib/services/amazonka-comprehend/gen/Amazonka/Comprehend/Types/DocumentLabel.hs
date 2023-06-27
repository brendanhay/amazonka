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
-- Module      : Amazonka.Comprehend.Types.DocumentLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentLabel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies one of the label or labels that categorize the document being
-- analyzed.
--
-- /See:/ 'newDocumentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { -- | The name of the label.
    name :: Prelude.Maybe Prelude.Text,
    -- | Page number where the label occurs. This field is present in the
    -- response only if your request includes the @Byte@ parameter.
    page :: Prelude.Maybe Prelude.Int,
    -- | The confidence score that Amazon Comprehend has this label correctly
    -- attributed.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentLabel_name' - The name of the label.
--
-- 'page', 'documentLabel_page' - Page number where the label occurs. This field is present in the
-- response only if your request includes the @Byte@ parameter.
--
-- 'score', 'documentLabel_score' - The confidence score that Amazon Comprehend has this label correctly
-- attributed.
newDocumentLabel ::
  DocumentLabel
newDocumentLabel =
  DocumentLabel'
    { name = Prelude.Nothing,
      page = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The name of the label.
documentLabel_name :: Lens.Lens' DocumentLabel (Prelude.Maybe Prelude.Text)
documentLabel_name = Lens.lens (\DocumentLabel' {name} -> name) (\s@DocumentLabel' {} a -> s {name = a} :: DocumentLabel)

-- | Page number where the label occurs. This field is present in the
-- response only if your request includes the @Byte@ parameter.
documentLabel_page :: Lens.Lens' DocumentLabel (Prelude.Maybe Prelude.Int)
documentLabel_page = Lens.lens (\DocumentLabel' {page} -> page) (\s@DocumentLabel' {} a -> s {page = a} :: DocumentLabel)

-- | The confidence score that Amazon Comprehend has this label correctly
-- attributed.
documentLabel_score :: Lens.Lens' DocumentLabel (Prelude.Maybe Prelude.Double)
documentLabel_score = Lens.lens (\DocumentLabel' {score} -> score) (\s@DocumentLabel' {} a -> s {score = a} :: DocumentLabel)

instance Data.FromJSON DocumentLabel where
  parseJSON =
    Data.withObject
      "DocumentLabel"
      ( \x ->
          DocumentLabel'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable DocumentLabel where
  hashWithSalt _salt DocumentLabel' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` score

instance Prelude.NFData DocumentLabel where
  rnf DocumentLabel' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf score

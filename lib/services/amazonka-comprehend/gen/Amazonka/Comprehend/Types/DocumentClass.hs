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
-- Module      : Amazonka.Comprehend.Types.DocumentClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClass where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the class that categorizes the document being analyzed
--
-- /See:/ 'newDocumentClass' smart constructor.
data DocumentClass = DocumentClass'
  { -- | The name of the class.
    name :: Prelude.Maybe Prelude.Text,
    -- | Page number in the input document. This field is present in the response
    -- only if your request includes the @Byte@ parameter.
    page :: Prelude.Maybe Prelude.Int,
    -- | The confidence score that Amazon Comprehend has this class correctly
    -- attributed.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClass' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentClass_name' - The name of the class.
--
-- 'page', 'documentClass_page' - Page number in the input document. This field is present in the response
-- only if your request includes the @Byte@ parameter.
--
-- 'score', 'documentClass_score' - The confidence score that Amazon Comprehend has this class correctly
-- attributed.
newDocumentClass ::
  DocumentClass
newDocumentClass =
  DocumentClass'
    { name = Prelude.Nothing,
      page = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The name of the class.
documentClass_name :: Lens.Lens' DocumentClass (Prelude.Maybe Prelude.Text)
documentClass_name = Lens.lens (\DocumentClass' {name} -> name) (\s@DocumentClass' {} a -> s {name = a} :: DocumentClass)

-- | Page number in the input document. This field is present in the response
-- only if your request includes the @Byte@ parameter.
documentClass_page :: Lens.Lens' DocumentClass (Prelude.Maybe Prelude.Int)
documentClass_page = Lens.lens (\DocumentClass' {page} -> page) (\s@DocumentClass' {} a -> s {page = a} :: DocumentClass)

-- | The confidence score that Amazon Comprehend has this class correctly
-- attributed.
documentClass_score :: Lens.Lens' DocumentClass (Prelude.Maybe Prelude.Double)
documentClass_score = Lens.lens (\DocumentClass' {score} -> score) (\s@DocumentClass' {} a -> s {score = a} :: DocumentClass)

instance Data.FromJSON DocumentClass where
  parseJSON =
    Data.withObject
      "DocumentClass"
      ( \x ->
          DocumentClass'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable DocumentClass where
  hashWithSalt _salt DocumentClass' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` score

instance Prelude.NFData DocumentClass where
  rnf DocumentClass' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf score

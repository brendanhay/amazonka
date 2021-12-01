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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentLabel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies one of the label or labels that categorize the document being
-- analyzed.
--
-- /See:/ 'newDocumentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { -- | The confidence score that Amazon Comprehend has this label correctly
    -- attributed.
    score :: Prelude.Maybe Prelude.Double,
    -- | The name of the label.
    name :: Prelude.Maybe Prelude.Text
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
-- 'score', 'documentLabel_score' - The confidence score that Amazon Comprehend has this label correctly
-- attributed.
--
-- 'name', 'documentLabel_name' - The name of the label.
newDocumentLabel ::
  DocumentLabel
newDocumentLabel =
  DocumentLabel'
    { score = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The confidence score that Amazon Comprehend has this label correctly
-- attributed.
documentLabel_score :: Lens.Lens' DocumentLabel (Prelude.Maybe Prelude.Double)
documentLabel_score = Lens.lens (\DocumentLabel' {score} -> score) (\s@DocumentLabel' {} a -> s {score = a} :: DocumentLabel)

-- | The name of the label.
documentLabel_name :: Lens.Lens' DocumentLabel (Prelude.Maybe Prelude.Text)
documentLabel_name = Lens.lens (\DocumentLabel' {name} -> name) (\s@DocumentLabel' {} a -> s {name = a} :: DocumentLabel)

instance Core.FromJSON DocumentLabel where
  parseJSON =
    Core.withObject
      "DocumentLabel"
      ( \x ->
          DocumentLabel'
            Prelude.<$> (x Core..:? "Score") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable DocumentLabel where
  hashWithSalt salt' DocumentLabel' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` score

instance Prelude.NFData DocumentLabel where
  rnf DocumentLabel' {..} =
    Prelude.rnf score `Prelude.seq` Prelude.rnf name

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
-- Module      : Network.AWS.Comprehend.Types.DocumentLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentLabel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies one of the label or labels that categorize the document being
-- analyzed.
--
-- /See:/ 'newDocumentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { -- | The name of the label.
    name :: Core.Maybe Core.Text,
    -- | The confidence score that Amazon Comprehend has this label correctly
    -- attributed.
    score :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'score', 'documentLabel_score' - The confidence score that Amazon Comprehend has this label correctly
-- attributed.
newDocumentLabel ::
  DocumentLabel
newDocumentLabel =
  DocumentLabel'
    { name = Core.Nothing,
      score = Core.Nothing
    }

-- | The name of the label.
documentLabel_name :: Lens.Lens' DocumentLabel (Core.Maybe Core.Text)
documentLabel_name = Lens.lens (\DocumentLabel' {name} -> name) (\s@DocumentLabel' {} a -> s {name = a} :: DocumentLabel)

-- | The confidence score that Amazon Comprehend has this label correctly
-- attributed.
documentLabel_score :: Lens.Lens' DocumentLabel (Core.Maybe Core.Double)
documentLabel_score = Lens.lens (\DocumentLabel' {score} -> score) (\s@DocumentLabel' {} a -> s {score = a} :: DocumentLabel)

instance Core.FromJSON DocumentLabel where
  parseJSON =
    Core.withObject
      "DocumentLabel"
      ( \x ->
          DocumentLabel'
            Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Score")
      )

instance Core.Hashable DocumentLabel

instance Core.NFData DocumentLabel

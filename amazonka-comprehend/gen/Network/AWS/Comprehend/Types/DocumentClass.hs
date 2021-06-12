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
-- Module      : Network.AWS.Comprehend.Types.DocumentClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClass where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the class that categorizes the document being analyzed
--
-- /See:/ 'newDocumentClass' smart constructor.
data DocumentClass = DocumentClass'
  { -- | The name of the class.
    name :: Core.Maybe Core.Text,
    -- | The confidence score that Amazon Comprehend has this class correctly
    -- attributed.
    score :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'score', 'documentClass_score' - The confidence score that Amazon Comprehend has this class correctly
-- attributed.
newDocumentClass ::
  DocumentClass
newDocumentClass =
  DocumentClass'
    { name = Core.Nothing,
      score = Core.Nothing
    }

-- | The name of the class.
documentClass_name :: Lens.Lens' DocumentClass (Core.Maybe Core.Text)
documentClass_name = Lens.lens (\DocumentClass' {name} -> name) (\s@DocumentClass' {} a -> s {name = a} :: DocumentClass)

-- | The confidence score that Amazon Comprehend has this class correctly
-- attributed.
documentClass_score :: Lens.Lens' DocumentClass (Core.Maybe Core.Double)
documentClass_score = Lens.lens (\DocumentClass' {score} -> score) (\s@DocumentClass' {} a -> s {score = a} :: DocumentClass)

instance Core.FromJSON DocumentClass where
  parseJSON =
    Core.withObject
      "DocumentClass"
      ( \x ->
          DocumentClass'
            Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Score")
      )

instance Core.Hashable DocumentClass

instance Core.NFData DocumentClass

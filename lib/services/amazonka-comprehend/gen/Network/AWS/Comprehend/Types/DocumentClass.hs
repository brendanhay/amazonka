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
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the class that categorizes the document being analyzed
--
-- /See:/ 'newDocumentClass' smart constructor.
data DocumentClass = DocumentClass'
  { -- | The confidence score that Amazon Comprehend has this class correctly
    -- attributed.
    score :: Prelude.Maybe Prelude.Double,
    -- | The name of the class.
    name :: Prelude.Maybe Prelude.Text
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
-- 'score', 'documentClass_score' - The confidence score that Amazon Comprehend has this class correctly
-- attributed.
--
-- 'name', 'documentClass_name' - The name of the class.
newDocumentClass ::
  DocumentClass
newDocumentClass =
  DocumentClass'
    { score = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The confidence score that Amazon Comprehend has this class correctly
-- attributed.
documentClass_score :: Lens.Lens' DocumentClass (Prelude.Maybe Prelude.Double)
documentClass_score = Lens.lens (\DocumentClass' {score} -> score) (\s@DocumentClass' {} a -> s {score = a} :: DocumentClass)

-- | The name of the class.
documentClass_name :: Lens.Lens' DocumentClass (Prelude.Maybe Prelude.Text)
documentClass_name = Lens.lens (\DocumentClass' {name} -> name) (\s@DocumentClass' {} a -> s {name = a} :: DocumentClass)

instance Core.FromJSON DocumentClass where
  parseJSON =
    Core.withObject
      "DocumentClass"
      ( \x ->
          DocumentClass'
            Prelude.<$> (x Core..:? "Score") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable DocumentClass

instance Prelude.NFData DocumentClass

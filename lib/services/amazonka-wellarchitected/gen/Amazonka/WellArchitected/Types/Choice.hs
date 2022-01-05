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
-- Module      : Amazonka.WellArchitected.Types.Choice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Choice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A choice available to answer question.
--
-- /See:/ 'newChoice' smart constructor.
data Choice = Choice'
  { title :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Choice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'choice_title' - Undocumented member.
--
-- 'description', 'choice_description' - Undocumented member.
--
-- 'choiceId', 'choice_choiceId' - Undocumented member.
newChoice ::
  Choice
newChoice =
  Choice'
    { title = Prelude.Nothing,
      description = Prelude.Nothing,
      choiceId = Prelude.Nothing
    }

-- | Undocumented member.
choice_title :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_title = Lens.lens (\Choice' {title} -> title) (\s@Choice' {} a -> s {title = a} :: Choice)

-- | Undocumented member.
choice_description :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_description = Lens.lens (\Choice' {description} -> description) (\s@Choice' {} a -> s {description = a} :: Choice)

-- | Undocumented member.
choice_choiceId :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_choiceId = Lens.lens (\Choice' {choiceId} -> choiceId) (\s@Choice' {} a -> s {choiceId = a} :: Choice)

instance Core.FromJSON Choice where
  parseJSON =
    Core.withObject
      "Choice"
      ( \x ->
          Choice'
            Prelude.<$> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ChoiceId")
      )

instance Prelude.Hashable Choice where
  hashWithSalt _salt Choice' {..} =
    _salt `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` choiceId

instance Prelude.NFData Choice where
  rnf Choice' {..} =
    Prelude.rnf title
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf choiceId

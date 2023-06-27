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
-- Module      : Amazonka.WellArchitected.Types.ProfileTemplateChoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileTemplateChoice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A profile template choice.
--
-- /See:/ 'newProfileTemplateChoice' smart constructor.
data ProfileTemplateChoice = ProfileTemplateChoice'
  { choiceDescription :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text,
    choiceTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileTemplateChoice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceDescription', 'profileTemplateChoice_choiceDescription' - Undocumented member.
--
-- 'choiceId', 'profileTemplateChoice_choiceId' - Undocumented member.
--
-- 'choiceTitle', 'profileTemplateChoice_choiceTitle' - Undocumented member.
newProfileTemplateChoice ::
  ProfileTemplateChoice
newProfileTemplateChoice =
  ProfileTemplateChoice'
    { choiceDescription =
        Prelude.Nothing,
      choiceId = Prelude.Nothing,
      choiceTitle = Prelude.Nothing
    }

-- | Undocumented member.
profileTemplateChoice_choiceDescription :: Lens.Lens' ProfileTemplateChoice (Prelude.Maybe Prelude.Text)
profileTemplateChoice_choiceDescription = Lens.lens (\ProfileTemplateChoice' {choiceDescription} -> choiceDescription) (\s@ProfileTemplateChoice' {} a -> s {choiceDescription = a} :: ProfileTemplateChoice)

-- | Undocumented member.
profileTemplateChoice_choiceId :: Lens.Lens' ProfileTemplateChoice (Prelude.Maybe Prelude.Text)
profileTemplateChoice_choiceId = Lens.lens (\ProfileTemplateChoice' {choiceId} -> choiceId) (\s@ProfileTemplateChoice' {} a -> s {choiceId = a} :: ProfileTemplateChoice)

-- | Undocumented member.
profileTemplateChoice_choiceTitle :: Lens.Lens' ProfileTemplateChoice (Prelude.Maybe Prelude.Text)
profileTemplateChoice_choiceTitle = Lens.lens (\ProfileTemplateChoice' {choiceTitle} -> choiceTitle) (\s@ProfileTemplateChoice' {} a -> s {choiceTitle = a} :: ProfileTemplateChoice)

instance Data.FromJSON ProfileTemplateChoice where
  parseJSON =
    Data.withObject
      "ProfileTemplateChoice"
      ( \x ->
          ProfileTemplateChoice'
            Prelude.<$> (x Data..:? "ChoiceDescription")
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "ChoiceTitle")
      )

instance Prelude.Hashable ProfileTemplateChoice where
  hashWithSalt _salt ProfileTemplateChoice' {..} =
    _salt
      `Prelude.hashWithSalt` choiceDescription
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` choiceTitle

instance Prelude.NFData ProfileTemplateChoice where
  rnf ProfileTemplateChoice' {..} =
    Prelude.rnf choiceDescription
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf choiceTitle

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
-- Module      : Amazonka.WellArchitected.Types.ProfileChoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileChoice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The profile choice.
--
-- /See:/ 'newProfileChoice' smart constructor.
data ProfileChoice = ProfileChoice'
  { choiceDescription :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text,
    choiceTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileChoice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceDescription', 'profileChoice_choiceDescription' - Undocumented member.
--
-- 'choiceId', 'profileChoice_choiceId' - Undocumented member.
--
-- 'choiceTitle', 'profileChoice_choiceTitle' - Undocumented member.
newProfileChoice ::
  ProfileChoice
newProfileChoice =
  ProfileChoice'
    { choiceDescription = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      choiceTitle = Prelude.Nothing
    }

-- | Undocumented member.
profileChoice_choiceDescription :: Lens.Lens' ProfileChoice (Prelude.Maybe Prelude.Text)
profileChoice_choiceDescription = Lens.lens (\ProfileChoice' {choiceDescription} -> choiceDescription) (\s@ProfileChoice' {} a -> s {choiceDescription = a} :: ProfileChoice)

-- | Undocumented member.
profileChoice_choiceId :: Lens.Lens' ProfileChoice (Prelude.Maybe Prelude.Text)
profileChoice_choiceId = Lens.lens (\ProfileChoice' {choiceId} -> choiceId) (\s@ProfileChoice' {} a -> s {choiceId = a} :: ProfileChoice)

-- | Undocumented member.
profileChoice_choiceTitle :: Lens.Lens' ProfileChoice (Prelude.Maybe Prelude.Text)
profileChoice_choiceTitle = Lens.lens (\ProfileChoice' {choiceTitle} -> choiceTitle) (\s@ProfileChoice' {} a -> s {choiceTitle = a} :: ProfileChoice)

instance Data.FromJSON ProfileChoice where
  parseJSON =
    Data.withObject
      "ProfileChoice"
      ( \x ->
          ProfileChoice'
            Prelude.<$> (x Data..:? "ChoiceDescription")
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "ChoiceTitle")
      )

instance Prelude.Hashable ProfileChoice where
  hashWithSalt _salt ProfileChoice' {..} =
    _salt
      `Prelude.hashWithSalt` choiceDescription
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` choiceTitle

instance Prelude.NFData ProfileChoice where
  rnf ProfileChoice' {..} =
    Prelude.rnf choiceDescription
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf choiceTitle

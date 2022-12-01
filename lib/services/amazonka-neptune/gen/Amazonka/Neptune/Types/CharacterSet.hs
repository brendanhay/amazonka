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
-- Module      : Amazonka.Neptune.Types.CharacterSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.CharacterSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a character set.
--
-- /See:/ 'newCharacterSet' smart constructor.
data CharacterSet = CharacterSet'
  { -- | The name of the character set.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | The description of the character set.
    characterSetDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CharacterSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'characterSetName', 'characterSet_characterSetName' - The name of the character set.
--
-- 'characterSetDescription', 'characterSet_characterSetDescription' - The description of the character set.
newCharacterSet ::
  CharacterSet
newCharacterSet =
  CharacterSet'
    { characterSetName = Prelude.Nothing,
      characterSetDescription = Prelude.Nothing
    }

-- | The name of the character set.
characterSet_characterSetName :: Lens.Lens' CharacterSet (Prelude.Maybe Prelude.Text)
characterSet_characterSetName = Lens.lens (\CharacterSet' {characterSetName} -> characterSetName) (\s@CharacterSet' {} a -> s {characterSetName = a} :: CharacterSet)

-- | The description of the character set.
characterSet_characterSetDescription :: Lens.Lens' CharacterSet (Prelude.Maybe Prelude.Text)
characterSet_characterSetDescription = Lens.lens (\CharacterSet' {characterSetDescription} -> characterSetDescription) (\s@CharacterSet' {} a -> s {characterSetDescription = a} :: CharacterSet)

instance Core.FromXML CharacterSet where
  parseXML x =
    CharacterSet'
      Prelude.<$> (x Core..@? "CharacterSetName")
      Prelude.<*> (x Core..@? "CharacterSetDescription")

instance Prelude.Hashable CharacterSet where
  hashWithSalt _salt CharacterSet' {..} =
    _salt `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` characterSetDescription

instance Prelude.NFData CharacterSet where
  rnf CharacterSet' {..} =
    Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf characterSetDescription

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.CharacterSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a character set.
--
-- /See:/ 'newCharacterSet' smart constructor.
data CharacterSet = CharacterSet'
  { -- | The description of the character set.
    characterSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the character set.
    characterSetName :: Prelude.Maybe Prelude.Text
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
-- 'characterSetDescription', 'characterSet_characterSetDescription' - The description of the character set.
--
-- 'characterSetName', 'characterSet_characterSetName' - The name of the character set.
newCharacterSet ::
  CharacterSet
newCharacterSet =
  CharacterSet'
    { characterSetDescription =
        Prelude.Nothing,
      characterSetName = Prelude.Nothing
    }

-- | The description of the character set.
characterSet_characterSetDescription :: Lens.Lens' CharacterSet (Prelude.Maybe Prelude.Text)
characterSet_characterSetDescription = Lens.lens (\CharacterSet' {characterSetDescription} -> characterSetDescription) (\s@CharacterSet' {} a -> s {characterSetDescription = a} :: CharacterSet)

-- | The name of the character set.
characterSet_characterSetName :: Lens.Lens' CharacterSet (Prelude.Maybe Prelude.Text)
characterSet_characterSetName = Lens.lens (\CharacterSet' {characterSetName} -> characterSetName) (\s@CharacterSet' {} a -> s {characterSetName = a} :: CharacterSet)

instance Data.FromXML CharacterSet where
  parseXML x =
    CharacterSet'
      Prelude.<$> (x Data..@? "CharacterSetDescription")
      Prelude.<*> (x Data..@? "CharacterSetName")

instance Prelude.Hashable CharacterSet where
  hashWithSalt _salt CharacterSet' {..} =
    _salt
      `Prelude.hashWithSalt` characterSetDescription
      `Prelude.hashWithSalt` characterSetName

instance Prelude.NFData CharacterSet where
  rnf CharacterSet' {..} =
    Prelude.rnf characterSetDescription
      `Prelude.seq` Prelude.rnf characterSetName

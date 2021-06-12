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
-- Module      : Network.AWS.RDS.Types.CharacterSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CharacterSet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used as a response element in the action
-- @DescribeDBEngineVersions@.
--
-- /See:/ 'newCharacterSet' smart constructor.
data CharacterSet = CharacterSet'
  { -- | The name of the character set.
    characterSetName :: Core.Maybe Core.Text,
    -- | The description of the character set.
    characterSetDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { characterSetName = Core.Nothing,
      characterSetDescription = Core.Nothing
    }

-- | The name of the character set.
characterSet_characterSetName :: Lens.Lens' CharacterSet (Core.Maybe Core.Text)
characterSet_characterSetName = Lens.lens (\CharacterSet' {characterSetName} -> characterSetName) (\s@CharacterSet' {} a -> s {characterSetName = a} :: CharacterSet)

-- | The description of the character set.
characterSet_characterSetDescription :: Lens.Lens' CharacterSet (Core.Maybe Core.Text)
characterSet_characterSetDescription = Lens.lens (\CharacterSet' {characterSetDescription} -> characterSetDescription) (\s@CharacterSet' {} a -> s {characterSetDescription = a} :: CharacterSet)

instance Core.FromXML CharacterSet where
  parseXML x =
    CharacterSet'
      Core.<$> (x Core..@? "CharacterSetName")
      Core.<*> (x Core..@? "CharacterSetDescription")

instance Core.Hashable CharacterSet

instance Core.NFData CharacterSet

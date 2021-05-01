{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELBv2.Types.Cipher
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Cipher where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a cipher used in a policy.
--
-- /See:/ 'newCipher' smart constructor.
data Cipher = Cipher'
  { -- | The priority of the cipher.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The name of the cipher.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Cipher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'cipher_priority' - The priority of the cipher.
--
-- 'name', 'cipher_name' - The name of the cipher.
newCipher ::
  Cipher
newCipher =
  Cipher'
    { priority = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The priority of the cipher.
cipher_priority :: Lens.Lens' Cipher (Prelude.Maybe Prelude.Int)
cipher_priority = Lens.lens (\Cipher' {priority} -> priority) (\s@Cipher' {} a -> s {priority = a} :: Cipher)

-- | The name of the cipher.
cipher_name :: Lens.Lens' Cipher (Prelude.Maybe Prelude.Text)
cipher_name = Lens.lens (\Cipher' {name} -> name) (\s@Cipher' {} a -> s {name = a} :: Cipher)

instance Prelude.FromXML Cipher where
  parseXML x =
    Cipher'
      Prelude.<$> (x Prelude..@? "Priority")
      Prelude.<*> (x Prelude..@? "Name")

instance Prelude.Hashable Cipher

instance Prelude.NFData Cipher

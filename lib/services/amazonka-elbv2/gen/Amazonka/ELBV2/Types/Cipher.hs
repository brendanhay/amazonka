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
-- Module      : Amazonka.ELBV2.Types.Cipher
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.Cipher where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a cipher used in a policy.
--
-- /See:/ 'newCipher' smart constructor.
data Cipher = Cipher'
  { -- | The name of the cipher.
    name :: Prelude.Maybe Prelude.Text,
    -- | The priority of the cipher.
    priority :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cipher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cipher_name' - The name of the cipher.
--
-- 'priority', 'cipher_priority' - The priority of the cipher.
newCipher ::
  Cipher
newCipher =
  Cipher'
    { name = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | The name of the cipher.
cipher_name :: Lens.Lens' Cipher (Prelude.Maybe Prelude.Text)
cipher_name = Lens.lens (\Cipher' {name} -> name) (\s@Cipher' {} a -> s {name = a} :: Cipher)

-- | The priority of the cipher.
cipher_priority :: Lens.Lens' Cipher (Prelude.Maybe Prelude.Int)
cipher_priority = Lens.lens (\Cipher' {priority} -> priority) (\s@Cipher' {} a -> s {priority = a} :: Cipher)

instance Data.FromXML Cipher where
  parseXML x =
    Cipher'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Priority")

instance Prelude.Hashable Cipher where
  hashWithSalt _salt Cipher' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority

instance Prelude.NFData Cipher where
  rnf Cipher' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf priority

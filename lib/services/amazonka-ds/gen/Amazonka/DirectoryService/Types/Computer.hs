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
-- Module      : Amazonka.DirectoryService.Types.Computer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.Computer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.Attribute
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'newComputer' smart constructor.
data Computer = Computer'
  { -- | An array of Attribute objects containing the LDAP attributes that belong
    -- to the computer account.
    computerAttributes :: Prelude.Maybe [Attribute],
    -- | The identifier of the computer.
    computerId :: Prelude.Maybe Prelude.Text,
    -- | The computer name.
    computerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Computer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computerAttributes', 'computer_computerAttributes' - An array of Attribute objects containing the LDAP attributes that belong
-- to the computer account.
--
-- 'computerId', 'computer_computerId' - The identifier of the computer.
--
-- 'computerName', 'computer_computerName' - The computer name.
newComputer ::
  Computer
newComputer =
  Computer'
    { computerAttributes = Prelude.Nothing,
      computerId = Prelude.Nothing,
      computerName = Prelude.Nothing
    }

-- | An array of Attribute objects containing the LDAP attributes that belong
-- to the computer account.
computer_computerAttributes :: Lens.Lens' Computer (Prelude.Maybe [Attribute])
computer_computerAttributes = Lens.lens (\Computer' {computerAttributes} -> computerAttributes) (\s@Computer' {} a -> s {computerAttributes = a} :: Computer) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the computer.
computer_computerId :: Lens.Lens' Computer (Prelude.Maybe Prelude.Text)
computer_computerId = Lens.lens (\Computer' {computerId} -> computerId) (\s@Computer' {} a -> s {computerId = a} :: Computer)

-- | The computer name.
computer_computerName :: Lens.Lens' Computer (Prelude.Maybe Prelude.Text)
computer_computerName = Lens.lens (\Computer' {computerName} -> computerName) (\s@Computer' {} a -> s {computerName = a} :: Computer)

instance Data.FromJSON Computer where
  parseJSON =
    Data.withObject
      "Computer"
      ( \x ->
          Computer'
            Prelude.<$> ( x
                            Data..:? "ComputerAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ComputerId")
            Prelude.<*> (x Data..:? "ComputerName")
      )

instance Prelude.Hashable Computer where
  hashWithSalt _salt Computer' {..} =
    _salt
      `Prelude.hashWithSalt` computerAttributes
      `Prelude.hashWithSalt` computerId
      `Prelude.hashWithSalt` computerName

instance Prelude.NFData Computer where
  rnf Computer' {..} =
    Prelude.rnf computerAttributes
      `Prelude.seq` Prelude.rnf computerId
      `Prelude.seq` Prelude.rnf computerName

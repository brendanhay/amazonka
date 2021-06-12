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
-- Module      : Network.AWS.DirectoryService.Types.Computer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Computer where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.Attribute
import qualified Network.AWS.Lens as Lens

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'newComputer' smart constructor.
data Computer = Computer'
  { -- | The computer name.
    computerName :: Core.Maybe Core.Text,
    -- | An array of Attribute objects containing the LDAP attributes that belong
    -- to the computer account.
    computerAttributes :: Core.Maybe [Attribute],
    -- | The identifier of the computer.
    computerId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Computer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computerName', 'computer_computerName' - The computer name.
--
-- 'computerAttributes', 'computer_computerAttributes' - An array of Attribute objects containing the LDAP attributes that belong
-- to the computer account.
--
-- 'computerId', 'computer_computerId' - The identifier of the computer.
newComputer ::
  Computer
newComputer =
  Computer'
    { computerName = Core.Nothing,
      computerAttributes = Core.Nothing,
      computerId = Core.Nothing
    }

-- | The computer name.
computer_computerName :: Lens.Lens' Computer (Core.Maybe Core.Text)
computer_computerName = Lens.lens (\Computer' {computerName} -> computerName) (\s@Computer' {} a -> s {computerName = a} :: Computer)

-- | An array of Attribute objects containing the LDAP attributes that belong
-- to the computer account.
computer_computerAttributes :: Lens.Lens' Computer (Core.Maybe [Attribute])
computer_computerAttributes = Lens.lens (\Computer' {computerAttributes} -> computerAttributes) (\s@Computer' {} a -> s {computerAttributes = a} :: Computer) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the computer.
computer_computerId :: Lens.Lens' Computer (Core.Maybe Core.Text)
computer_computerId = Lens.lens (\Computer' {computerId} -> computerId) (\s@Computer' {} a -> s {computerId = a} :: Computer)

instance Core.FromJSON Computer where
  parseJSON =
    Core.withObject
      "Computer"
      ( \x ->
          Computer'
            Core.<$> (x Core..:? "ComputerName")
            Core.<*> ( x Core..:? "ComputerAttributes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ComputerId")
      )

instance Core.Hashable Computer

instance Core.NFData Computer

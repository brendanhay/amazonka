{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Computer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Computer
  ( Computer (..),

    -- * Smart constructor
    mkComputer,

    -- * Lenses
    cComputerAttributes,
    cComputerId,
    cComputerName,
  )
where

import qualified Network.AWS.DirectoryService.Types.Attribute as Types
import qualified Network.AWS.DirectoryService.Types.ComputerId as Types
import qualified Network.AWS.DirectoryService.Types.ComputerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'mkComputer' smart constructor.
data Computer = Computer'
  { -- | An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
    computerAttributes :: Core.Maybe [Types.Attribute],
    -- | The identifier of the computer.
    computerId :: Core.Maybe Types.ComputerId,
    -- | The computer name.
    computerName :: Core.Maybe Types.ComputerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Computer' value with any optional fields omitted.
mkComputer ::
  Computer
mkComputer =
  Computer'
    { computerAttributes = Core.Nothing,
      computerId = Core.Nothing,
      computerName = Core.Nothing
    }

-- | An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
--
-- /Note:/ Consider using 'computerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerAttributes :: Lens.Lens' Computer (Core.Maybe [Types.Attribute])
cComputerAttributes = Lens.field @"computerAttributes"
{-# DEPRECATED cComputerAttributes "Use generic-lens or generic-optics with 'computerAttributes' instead." #-}

-- | The identifier of the computer.
--
-- /Note:/ Consider using 'computerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerId :: Lens.Lens' Computer (Core.Maybe Types.ComputerId)
cComputerId = Lens.field @"computerId"
{-# DEPRECATED cComputerId "Use generic-lens or generic-optics with 'computerId' instead." #-}

-- | The computer name.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerName :: Lens.Lens' Computer (Core.Maybe Types.ComputerName)
cComputerName = Lens.field @"computerName"
{-# DEPRECATED cComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

instance Core.FromJSON Computer where
  parseJSON =
    Core.withObject "Computer" Core.$
      \x ->
        Computer'
          Core.<$> (x Core..:? "ComputerAttributes")
          Core.<*> (x Core..:? "ComputerId")
          Core.<*> (x Core..:? "ComputerName")

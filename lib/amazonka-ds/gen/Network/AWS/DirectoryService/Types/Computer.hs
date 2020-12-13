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
    cComputerId,
    cComputerAttributes,
    cComputerName,
  )
where

import Network.AWS.DirectoryService.Types.Attribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'mkComputer' smart constructor.
data Computer = Computer'
  { -- | The identifier of the computer.
    computerId :: Lude.Maybe Lude.Text,
    -- | An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
    computerAttributes :: Lude.Maybe [Attribute],
    -- | The computer name.
    computerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Computer' with the minimum fields required to make a request.
--
-- * 'computerId' - The identifier of the computer.
-- * 'computerAttributes' - An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
-- * 'computerName' - The computer name.
mkComputer ::
  Computer
mkComputer =
  Computer'
    { computerId = Lude.Nothing,
      computerAttributes = Lude.Nothing,
      computerName = Lude.Nothing
    }

-- | The identifier of the computer.
--
-- /Note:/ Consider using 'computerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerId :: Lens.Lens' Computer (Lude.Maybe Lude.Text)
cComputerId = Lens.lens (computerId :: Computer -> Lude.Maybe Lude.Text) (\s a -> s {computerId = a} :: Computer)
{-# DEPRECATED cComputerId "Use generic-lens or generic-optics with 'computerId' instead." #-}

-- | An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
--
-- /Note:/ Consider using 'computerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerAttributes :: Lens.Lens' Computer (Lude.Maybe [Attribute])
cComputerAttributes = Lens.lens (computerAttributes :: Computer -> Lude.Maybe [Attribute]) (\s a -> s {computerAttributes = a} :: Computer)
{-# DEPRECATED cComputerAttributes "Use generic-lens or generic-optics with 'computerAttributes' instead." #-}

-- | The computer name.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComputerName :: Lens.Lens' Computer (Lude.Maybe Lude.Text)
cComputerName = Lens.lens (computerName :: Computer -> Lude.Maybe Lude.Text) (\s a -> s {computerName = a} :: Computer)
{-# DEPRECATED cComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

instance Lude.FromJSON Computer where
  parseJSON =
    Lude.withObject
      "Computer"
      ( \x ->
          Computer'
            Lude.<$> (x Lude..:? "ComputerId")
            Lude.<*> (x Lude..:? "ComputerAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ComputerName")
      )

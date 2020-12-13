{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AddPrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddPrefixListEntry
  ( AddPrefixListEntry (..),

    -- * Smart constructor
    mkAddPrefixListEntry,

    -- * Lenses
    apleCidr,
    apleDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entry for a prefix list.
--
-- /See:/ 'mkAddPrefixListEntry' smart constructor.
data AddPrefixListEntry = AddPrefixListEntry'
  { -- | The CIDR block.
    cidr :: Lude.Text,
    -- | A description for the entry.
    --
    -- Constraints: Up to 255 characters in length.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPrefixListEntry' with the minimum fields required to make a request.
--
-- * 'cidr' - The CIDR block.
-- * 'description' - A description for the entry.
--
-- Constraints: Up to 255 characters in length.
mkAddPrefixListEntry ::
  -- | 'cidr'
  Lude.Text ->
  AddPrefixListEntry
mkAddPrefixListEntry pCidr_ =
  AddPrefixListEntry' {cidr = pCidr_, description = Lude.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apleCidr :: Lens.Lens' AddPrefixListEntry Lude.Text
apleCidr = Lens.lens (cidr :: AddPrefixListEntry -> Lude.Text) (\s a -> s {cidr = a} :: AddPrefixListEntry)
{-# DEPRECATED apleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | A description for the entry.
--
-- Constraints: Up to 255 characters in length.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apleDescription :: Lens.Lens' AddPrefixListEntry (Lude.Maybe Lude.Text)
apleDescription = Lens.lens (description :: AddPrefixListEntry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AddPrefixListEntry)
{-# DEPRECATED apleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToQuery AddPrefixListEntry where
  toQuery AddPrefixListEntry' {..} =
    Lude.mconcat
      ["Cidr" Lude.=: cidr, "Description" Lude.=: description]

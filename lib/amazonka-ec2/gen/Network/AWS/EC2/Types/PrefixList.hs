{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixList
  ( PrefixList (..),

    -- * Smart constructor
    mkPrefixList,

    -- * Lenses
    plCidrs,
    plPrefixListId,
    plPrefixListName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes prefixes for AWS services.
--
-- /See:/ 'mkPrefixList' smart constructor.
data PrefixList = PrefixList'
  { -- | The IP address range of the AWS service.
    cidrs :: Lude.Maybe [Lude.Text],
    -- | The ID of the prefix.
    prefixListId :: Lude.Maybe Lude.Text,
    -- | The name of the prefix.
    prefixListName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrefixList' with the minimum fields required to make a request.
--
-- * 'cidrs' - The IP address range of the AWS service.
-- * 'prefixListId' - The ID of the prefix.
-- * 'prefixListName' - The name of the prefix.
mkPrefixList ::
  PrefixList
mkPrefixList =
  PrefixList'
    { cidrs = Lude.Nothing,
      prefixListId = Lude.Nothing,
      prefixListName = Lude.Nothing
    }

-- | The IP address range of the AWS service.
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plCidrs :: Lens.Lens' PrefixList (Lude.Maybe [Lude.Text])
plCidrs = Lens.lens (cidrs :: PrefixList -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrs = a} :: PrefixList)
{-# DEPRECATED plCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

-- | The ID of the prefix.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPrefixListId :: Lens.Lens' PrefixList (Lude.Maybe Lude.Text)
plPrefixListId = Lens.lens (prefixListId :: PrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: PrefixList)
{-# DEPRECATED plPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The name of the prefix.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPrefixListName :: Lens.Lens' PrefixList (Lude.Maybe Lude.Text)
plPrefixListName = Lens.lens (prefixListName :: PrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListName = a} :: PrefixList)
{-# DEPRECATED plPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

instance Lude.FromXML PrefixList where
  parseXML x =
    PrefixList'
      Lude.<$> ( x Lude..@? "cidrSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "prefixListId")
      Lude.<*> (x Lude..@? "prefixListName")

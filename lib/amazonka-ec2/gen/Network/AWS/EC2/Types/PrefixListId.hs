{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListId
  ( PrefixListId (..),

    -- * Smart constructor
    mkPrefixListId,

    -- * Lenses
    pliPrefixListId,
    pliDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a prefix list ID.
--
-- /See:/ 'mkPrefixListId' smart constructor.
data PrefixListId = PrefixListId'
  { prefixListId ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrefixListId' with the minimum fields required to make a request.
--
-- * 'description' - A description for the security group rule that references this prefix list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
-- * 'prefixListId' - The ID of the prefix.
mkPrefixListId ::
  PrefixListId
mkPrefixListId =
  PrefixListId'
    { prefixListId = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of the prefix.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pliPrefixListId :: Lens.Lens' PrefixListId (Lude.Maybe Lude.Text)
pliPrefixListId = Lens.lens (prefixListId :: PrefixListId -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: PrefixListId)
{-# DEPRECATED pliPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | A description for the security group rule that references this prefix list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pliDescription :: Lens.Lens' PrefixListId (Lude.Maybe Lude.Text)
pliDescription = Lens.lens (description :: PrefixListId -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PrefixListId)
{-# DEPRECATED pliDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML PrefixListId where
  parseXML x =
    PrefixListId'
      Lude.<$> (x Lude..@? "prefixListId") Lude.<*> (x Lude..@? "description")

instance Lude.ToQuery PrefixListId where
  toQuery PrefixListId' {..} =
    Lude.mconcat
      [ "PrefixListId" Lude.=: prefixListId,
        "Description" Lude.=: description
      ]

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Owner
  ( Owner (..),

    -- * Smart constructor
    mkOwner,

    -- * Lenses
    oId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the owner of the bucket.
--
-- /See:/ 'mkOwner' smart constructor.
newtype Owner = Owner' {id :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- * 'id' - The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
mkOwner ::
  Owner
mkOwner = Owner' {id = Lude.Nothing}

-- | The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Owner (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Owner -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Owner)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Owner where
  parseJSON =
    Lude.withObject
      "Owner"
      (\x -> Owner' Lude.<$> (x Lude..:? "id"))

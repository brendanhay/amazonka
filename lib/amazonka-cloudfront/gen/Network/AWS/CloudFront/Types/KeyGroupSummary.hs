-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupSummary
  ( KeyGroupSummary (..),

    -- * Smart constructor
    mkKeyGroupSummary,

    -- * Lenses
    kgsKeyGroup,
  )
where

import Network.AWS.CloudFront.Types.KeyGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a key group.
--
-- /See:/ 'mkKeyGroupSummary' smart constructor.
newtype KeyGroupSummary = KeyGroupSummary' {keyGroup :: KeyGroup}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyGroupSummary' with the minimum fields required to make a request.
--
-- * 'keyGroup' - A key group.
mkKeyGroupSummary ::
  -- | 'keyGroup'
  KeyGroup ->
  KeyGroupSummary
mkKeyGroupSummary pKeyGroup_ =
  KeyGroupSummary' {keyGroup = pKeyGroup_}

-- | A key group.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgsKeyGroup :: Lens.Lens' KeyGroupSummary KeyGroup
kgsKeyGroup = Lens.lens (keyGroup :: KeyGroupSummary -> KeyGroup) (\s a -> s {keyGroup = a} :: KeyGroupSummary)
{-# DEPRECATED kgsKeyGroup "Use generic-lens or generic-optics with 'keyGroup' instead." #-}

instance Lude.FromXML KeyGroupSummary where
  parseXML x = KeyGroupSummary' Lude.<$> (x Lude..@ "KeyGroup")

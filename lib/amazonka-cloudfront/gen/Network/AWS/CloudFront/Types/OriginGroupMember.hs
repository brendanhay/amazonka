-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMember
  ( OriginGroupMember (..),

    -- * Smart constructor
    mkOriginGroupMember,

    -- * Lenses
    ogmOriginId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An origin in an origin group.
--
-- /See:/ 'mkOriginGroupMember' smart constructor.
newtype OriginGroupMember = OriginGroupMember'
  { originId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginGroupMember' with the minimum fields required to make a request.
--
-- * 'originId' - The ID for an origin in an origin group.
mkOriginGroupMember ::
  -- | 'originId'
  Lude.Text ->
  OriginGroupMember
mkOriginGroupMember pOriginId_ =
  OriginGroupMember' {originId = pOriginId_}

-- | The ID for an origin in an origin group.
--
-- /Note:/ Consider using 'originId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmOriginId :: Lens.Lens' OriginGroupMember Lude.Text
ogmOriginId = Lens.lens (originId :: OriginGroupMember -> Lude.Text) (\s a -> s {originId = a} :: OriginGroupMember)
{-# DEPRECATED ogmOriginId "Use generic-lens or generic-optics with 'originId' instead." #-}

instance Lude.FromXML OriginGroupMember where
  parseXML x = OriginGroupMember' Lude.<$> (x Lude..@ "OriginId")

instance Lude.ToXML OriginGroupMember where
  toXML OriginGroupMember' {..} =
    Lude.mconcat ["OriginId" Lude.@= originId]

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagKeyOnly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagKeyOnly
  ( TagKeyOnly (..),

    -- * Smart constructor
    mkTagKeyOnly,

    -- * Lenses
    tkoKey,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The key of a tag.
--
-- /See:/ 'mkTagKeyOnly' smart constructor.
newtype TagKeyOnly = TagKeyOnly'
  { -- | The name of the key.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagKeyOnly' with the minimum fields required to make a request.
--
-- * 'key' - The name of the key.
mkTagKeyOnly ::
  TagKeyOnly
mkTagKeyOnly = TagKeyOnly' {key = Lude.Nothing}

-- | The name of the key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkoKey :: Lens.Lens' TagKeyOnly (Lude.Maybe Lude.Text)
tkoKey = Lens.lens (key :: TagKeyOnly -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagKeyOnly)
{-# DEPRECATED tkoKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToQuery TagKeyOnly where
  toQuery TagKeyOnly' {..} = Lude.mconcat ["Key" Lude.=: key]

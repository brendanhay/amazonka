{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeBatch
  ( ChangeBatch (..),

    -- * Smart constructor
    mkChangeBatch,

    -- * Lenses
    cbChanges,
    cbComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Change

-- | The information for a change request.
--
-- /See:/ 'mkChangeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { -- | Information about the changes to make to the record sets.
    changes :: Lude.NonEmpty Change,
    -- | /Optional:/ Any comments you want to include about a change batch request.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeBatch' with the minimum fields required to make a request.
--
-- * 'changes' - Information about the changes to make to the record sets.
-- * 'comment' - /Optional:/ Any comments you want to include about a change batch request.
mkChangeBatch ::
  -- | 'changes'
  Lude.NonEmpty Change ->
  ChangeBatch
mkChangeBatch pChanges_ =
  ChangeBatch' {changes = pChanges_, comment = Lude.Nothing}

-- | Information about the changes to make to the record sets.
--
-- /Note:/ Consider using 'changes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbChanges :: Lens.Lens' ChangeBatch (Lude.NonEmpty Change)
cbChanges = Lens.lens (changes :: ChangeBatch -> Lude.NonEmpty Change) (\s a -> s {changes = a} :: ChangeBatch)
{-# DEPRECATED cbChanges "Use generic-lens or generic-optics with 'changes' instead." #-}

-- | /Optional:/ Any comments you want to include about a change batch request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbComment :: Lens.Lens' ChangeBatch (Lude.Maybe Lude.Text)
cbComment = Lens.lens (comment :: ChangeBatch -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: ChangeBatch)
{-# DEPRECATED cbComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.ToXML ChangeBatch where
  toXML ChangeBatch' {..} =
    Lude.mconcat
      [ "Changes" Lude.@= Lude.toXMLList "Change" changes,
        "Comment" Lude.@= comment
      ]

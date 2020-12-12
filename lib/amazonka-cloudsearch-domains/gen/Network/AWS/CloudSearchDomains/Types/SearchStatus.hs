{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SearchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SearchStatus
  ( SearchStatus (..),

    -- * Smart constructor
    mkSearchStatus,

    -- * Lenses
    sRid,
    sTimems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
-- /See:/ 'mkSearchStatus' smart constructor.
data SearchStatus = SearchStatus'
  { rid :: Lude.Maybe Lude.Text,
    timems :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchStatus' with the minimum fields required to make a request.
--
-- * 'rid' - The encrypted resource ID for the request.
-- * 'timems' - How long it took to process the request, in milliseconds.
mkSearchStatus ::
  SearchStatus
mkSearchStatus =
  SearchStatus' {rid = Lude.Nothing, timems = Lude.Nothing}

-- | The encrypted resource ID for the request.
--
-- /Note:/ Consider using 'rid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRid :: Lens.Lens' SearchStatus (Lude.Maybe Lude.Text)
sRid = Lens.lens (rid :: SearchStatus -> Lude.Maybe Lude.Text) (\s a -> s {rid = a} :: SearchStatus)
{-# DEPRECATED sRid "Use generic-lens or generic-optics with 'rid' instead." #-}

-- | How long it took to process the request, in milliseconds.
--
-- /Note:/ Consider using 'timems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimems :: Lens.Lens' SearchStatus (Lude.Maybe Lude.Integer)
sTimems = Lens.lens (timems :: SearchStatus -> Lude.Maybe Lude.Integer) (\s a -> s {timems = a} :: SearchStatus)
{-# DEPRECATED sTimems "Use generic-lens or generic-optics with 'timems' instead." #-}

instance Lude.FromJSON SearchStatus where
  parseJSON =
    Lude.withObject
      "SearchStatus"
      ( \x ->
          SearchStatus'
            Lude.<$> (x Lude..:? "rid") Lude.<*> (x Lude..:? "timems")
      )

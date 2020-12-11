-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestStatus
  ( SuggestStatus (..),

    -- * Smart constructor
    mkSuggestStatus,

    -- * Lenses
    ssRid,
    ssTimems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
-- /See:/ 'mkSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
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

-- | Creates a value of 'SuggestStatus' with the minimum fields required to make a request.
--
-- * 'rid' - The encrypted resource ID for the request.
-- * 'timems' - How long it took to process the request, in milliseconds.
mkSuggestStatus ::
  SuggestStatus
mkSuggestStatus =
  SuggestStatus' {rid = Lude.Nothing, timems = Lude.Nothing}

-- | The encrypted resource ID for the request.
--
-- /Note:/ Consider using 'rid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRid :: Lens.Lens' SuggestStatus (Lude.Maybe Lude.Text)
ssRid = Lens.lens (rid :: SuggestStatus -> Lude.Maybe Lude.Text) (\s a -> s {rid = a} :: SuggestStatus)
{-# DEPRECATED ssRid "Use generic-lens or generic-optics with 'rid' instead." #-}

-- | How long it took to process the request, in milliseconds.
--
-- /Note:/ Consider using 'timems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimems :: Lens.Lens' SuggestStatus (Lude.Maybe Lude.Integer)
ssTimems = Lens.lens (timems :: SuggestStatus -> Lude.Maybe Lude.Integer) (\s a -> s {timems = a} :: SuggestStatus)
{-# DEPRECATED ssTimems "Use generic-lens or generic-optics with 'timems' instead." #-}

instance Lude.FromJSON SuggestStatus where
  parseJSON =
    Lude.withObject
      "SuggestStatus"
      ( \x ->
          SuggestStatus'
            Lude.<$> (x Lude..:? "rid") Lude.<*> (x Lude..:? "timems")
      )

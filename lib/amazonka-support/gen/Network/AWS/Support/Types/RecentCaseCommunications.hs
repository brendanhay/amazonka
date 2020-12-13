{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.RecentCaseCommunications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.RecentCaseCommunications
  ( RecentCaseCommunications (..),

    -- * Smart constructor
    mkRecentCaseCommunications,

    -- * Lenses
    rccNextToken,
    rccCommunications,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.Communication

-- | The five most recent communications associated with the case.
--
-- /See:/ 'mkRecentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
  { -- | A resumption point for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The five most recent communications associated with the case.
    communications :: Lude.Maybe [Communication]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecentCaseCommunications' with the minimum fields required to make a request.
--
-- * 'nextToken' - A resumption point for pagination.
-- * 'communications' - The five most recent communications associated with the case.
mkRecentCaseCommunications ::
  RecentCaseCommunications
mkRecentCaseCommunications =
  RecentCaseCommunications'
    { nextToken = Lude.Nothing,
      communications = Lude.Nothing
    }

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccNextToken :: Lens.Lens' RecentCaseCommunications (Lude.Maybe Lude.Text)
rccNextToken = Lens.lens (nextToken :: RecentCaseCommunications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: RecentCaseCommunications)
{-# DEPRECATED rccNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The five most recent communications associated with the case.
--
-- /Note:/ Consider using 'communications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCommunications :: Lens.Lens' RecentCaseCommunications (Lude.Maybe [Communication])
rccCommunications = Lens.lens (communications :: RecentCaseCommunications -> Lude.Maybe [Communication]) (\s a -> s {communications = a} :: RecentCaseCommunications)
{-# DEPRECATED rccCommunications "Use generic-lens or generic-optics with 'communications' instead." #-}

instance Lude.FromJSON RecentCaseCommunications where
  parseJSON =
    Lude.withObject
      "RecentCaseCommunications"
      ( \x ->
          RecentCaseCommunications'
            Lude.<$> (x Lude..:? "nextToken")
            Lude.<*> (x Lude..:? "communications" Lude..!= Lude.mempty)
      )

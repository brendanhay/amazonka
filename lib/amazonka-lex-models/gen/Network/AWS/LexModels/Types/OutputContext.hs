{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.OutputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.OutputContext
  ( OutputContext (..),

    -- * Smart constructor
    mkOutputContext,

    -- * Lenses
    ocTurnsToLive,
    ocTimeToLiveInSeconds,
    ocName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The specification of an output context that is set when an intent is fulfilled.
--
-- /See:/ 'mkOutputContext' smart constructor.
data OutputContext = OutputContext'
  { -- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
    turnsToLive :: Lude.Natural,
    -- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
    timeToLiveInSeconds :: Lude.Natural,
    -- | The name of the context.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputContext' with the minimum fields required to make a request.
--
-- * 'turnsToLive' - The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
-- * 'timeToLiveInSeconds' - The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
-- * 'name' - The name of the context.
mkOutputContext ::
  -- | 'turnsToLive'
  Lude.Natural ->
  -- | 'timeToLiveInSeconds'
  Lude.Natural ->
  -- | 'name'
  Lude.Text ->
  OutputContext
mkOutputContext pTurnsToLive_ pTimeToLiveInSeconds_ pName_ =
  OutputContext'
    { turnsToLive = pTurnsToLive_,
      timeToLiveInSeconds = pTimeToLiveInSeconds_,
      name = pName_
    }

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
--
-- /Note:/ Consider using 'turnsToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTurnsToLive :: Lens.Lens' OutputContext Lude.Natural
ocTurnsToLive = Lens.lens (turnsToLive :: OutputContext -> Lude.Natural) (\s a -> s {turnsToLive = a} :: OutputContext)
{-# DEPRECATED ocTurnsToLive "Use generic-lens or generic-optics with 'turnsToLive' instead." #-}

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTimeToLiveInSeconds :: Lens.Lens' OutputContext Lude.Natural
ocTimeToLiveInSeconds = Lens.lens (timeToLiveInSeconds :: OutputContext -> Lude.Natural) (\s a -> s {timeToLiveInSeconds = a} :: OutputContext)
{-# DEPRECATED ocTimeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead." #-}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocName :: Lens.Lens' OutputContext Lude.Text
ocName = Lens.lens (name :: OutputContext -> Lude.Text) (\s a -> s {name = a} :: OutputContext)
{-# DEPRECATED ocName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON OutputContext where
  parseJSON =
    Lude.withObject
      "OutputContext"
      ( \x ->
          OutputContext'
            Lude.<$> (x Lude..: "turnsToLive")
            Lude.<*> (x Lude..: "timeToLiveInSeconds")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON OutputContext where
  toJSON OutputContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("turnsToLive" Lude..= turnsToLive),
            Lude.Just ("timeToLiveInSeconds" Lude..= timeToLiveInSeconds),
            Lude.Just ("name" Lude..= name)
          ]
      )

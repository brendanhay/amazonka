{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ActiveContext
  ( ActiveContext (..),

    -- * Smart constructor
    mkActiveContext,

    -- * Lenses
    acTimeToLive,
    acName,
    acParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import qualified Network.AWS.Prelude as Lude

-- | A context is a variable that contains information about the current state of the conversation between a user and Amazon Lex. Context can be set automatically by Amazon Lex when an intent is fulfilled, or it can be set at runtime using the @PutContent@ , @PutText@ , or @PutSession@ operation.
--
-- /See:/ 'mkActiveContext' smart constructor.
data ActiveContext = ActiveContext'
  { -- | The length of time or number of turns that a context remains active.
    timeToLive :: ActiveContextTimeToLive,
    -- | The name of the context.
    name :: Lude.Text,
    -- | State variables for the current context. You can use these values as default values for slots in subsequent events.
    parameters :: Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveContext' with the minimum fields required to make a request.
--
-- * 'timeToLive' - The length of time or number of turns that a context remains active.
-- * 'name' - The name of the context.
-- * 'parameters' - State variables for the current context. You can use these values as default values for slots in subsequent events.
mkActiveContext ::
  -- | 'timeToLive'
  ActiveContextTimeToLive ->
  -- | 'name'
  Lude.Text ->
  ActiveContext
mkActiveContext pTimeToLive_ pName_ =
  ActiveContext'
    { timeToLive = pTimeToLive_,
      name = pName_,
      parameters = Lude.mempty
    }

-- | The length of time or number of turns that a context remains active.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTimeToLive :: Lens.Lens' ActiveContext ActiveContextTimeToLive
acTimeToLive = Lens.lens (timeToLive :: ActiveContext -> ActiveContextTimeToLive) (\s a -> s {timeToLive = a} :: ActiveContext)
{-# DEPRECATED acTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' ActiveContext Lude.Text
acName = Lens.lens (name :: ActiveContext -> Lude.Text) (\s a -> s {name = a} :: ActiveContext)
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | State variables for the current context. You can use these values as default values for slots in subsequent events.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acParameters :: Lens.Lens' ActiveContext (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text))
acParameters = Lens.lens (parameters :: ActiveContext -> Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)) (\s a -> s {parameters = a} :: ActiveContext)
{-# DEPRECATED acParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.FromJSON ActiveContext where
  parseJSON =
    Lude.withObject
      "ActiveContext"
      ( \x ->
          ActiveContext'
            Lude.<$> (x Lude..: "timeToLive")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("timeToLive" Lude..= timeToLive),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("parameters" Lude..= parameters)
          ]
      )

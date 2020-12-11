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
    acName,
    acTimeToLive,
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
  { name :: Lude.Text,
    timeToLive :: ActiveContextTimeToLive,
    parameters :: Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveContext' with the minimum fields required to make a request.
--
-- * 'name' - The name of the context.
-- * 'parameters' - State variables for the current context. You can use these values as default values for slots in subsequent events.
-- * 'timeToLive' - The length of time or number of turns that a context remains active.
mkActiveContext ::
  -- | 'name'
  Lude.Text ->
  -- | 'timeToLive'
  ActiveContextTimeToLive ->
  ActiveContext
mkActiveContext pName_ pTimeToLive_ =
  ActiveContext'
    { name = pName_,
      timeToLive = pTimeToLive_,
      parameters = Lude.mempty
    }

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' ActiveContext Lude.Text
acName = Lens.lens (name :: ActiveContext -> Lude.Text) (\s a -> s {name = a} :: ActiveContext)
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The length of time or number of turns that a context remains active.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTimeToLive :: Lens.Lens' ActiveContext ActiveContextTimeToLive
acTimeToLive = Lens.lens (timeToLive :: ActiveContext -> ActiveContextTimeToLive) (\s a -> s {timeToLive = a} :: ActiveContext)
{-# DEPRECATED acTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

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
            Lude.<$> (x Lude..: "name")
            Lude.<*> (x Lude..: "timeToLive")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("timeToLive" Lude..= timeToLive),
            Lude.Just ("parameters" Lude..= parameters)
          ]
      )

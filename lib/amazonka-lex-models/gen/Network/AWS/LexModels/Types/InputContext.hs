-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.InputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.InputContext
  ( InputContext (..),

    -- * Smart constructor
    mkInputContext,

    -- * Lenses
    icName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The name of a context that must be active for an intent to be selected by Amazon Lex.
--
-- /See:/ 'mkInputContext' smart constructor.
newtype InputContext = InputContext' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputContext' with the minimum fields required to make a request.
--
-- * 'name' - The name of the context.
mkInputContext ::
  -- | 'name'
  Lude.Text ->
  InputContext
mkInputContext pName_ = InputContext' {name = pName_}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icName :: Lens.Lens' InputContext Lude.Text
icName = Lens.lens (name :: InputContext -> Lude.Text) (\s a -> s {name = a} :: InputContext)
{-# DEPRECATED icName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON InputContext where
  parseJSON =
    Lude.withObject
      "InputContext"
      (\x -> InputContext' Lude.<$> (x Lude..: "name"))

instance Lude.ToJSON InputContext where
  toJSON InputContext' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

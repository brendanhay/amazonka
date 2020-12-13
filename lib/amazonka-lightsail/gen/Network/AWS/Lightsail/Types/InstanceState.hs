{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceState
  ( InstanceState (..),

    -- * Smart constructor
    mkInstanceState,

    -- * Lenses
    isfName,
    isfCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the virtual private server (or /instance/ ) status.
--
-- /See:/ 'mkInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The state of the instance (e.g., @running@ or @pending@ ).
    name :: Lude.Maybe Lude.Text,
    -- | The status code for the instance.
    code :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- * 'name' - The state of the instance (e.g., @running@ or @pending@ ).
-- * 'code' - The status code for the instance.
mkInstanceState ::
  InstanceState
mkInstanceState =
  InstanceState' {name = Lude.Nothing, code = Lude.Nothing}

-- | The state of the instance (e.g., @running@ or @pending@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfName :: Lens.Lens' InstanceState (Lude.Maybe Lude.Text)
isfName = Lens.lens (name :: InstanceState -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceState)
{-# DEPRECATED isfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status code for the instance.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfCode :: Lens.Lens' InstanceState (Lude.Maybe Lude.Int)
isfCode = Lens.lens (code :: InstanceState -> Lude.Maybe Lude.Int) (\s a -> s {code = a} :: InstanceState)
{-# DEPRECATED isfCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON InstanceState where
  parseJSON =
    Lude.withObject
      "InstanceState"
      ( \x ->
          InstanceState'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "code")
      )

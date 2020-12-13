{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Trigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Trigger
  ( Trigger (..),

    -- * Smart constructor
    mkTrigger,

    -- * Lenses
    tName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a trigger.
--
-- /See:/ 'mkTrigger' smart constructor.
newtype Trigger = Trigger'
  { -- | The name of the trigger.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger.
mkTrigger ::
  Trigger
mkTrigger = Trigger' {name = Lude.Nothing}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tName = Lens.lens (name :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Trigger)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Trigger where
  parseXML x = Trigger' Lude.<$> (x Lude..@? "Name")

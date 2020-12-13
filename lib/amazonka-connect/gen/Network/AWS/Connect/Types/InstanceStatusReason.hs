{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatusReason
  ( InstanceStatusReason (..),

    -- * Smart constructor
    mkInstanceStatusReason,

    -- * Lenses
    isrMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Relevant details why the instance was not successfully created.
--
-- /See:/ 'mkInstanceStatusReason' smart constructor.
newtype InstanceStatusReason = InstanceStatusReason'
  { -- | The message.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStatusReason' with the minimum fields required to make a request.
--
-- * 'message' - The message.
mkInstanceStatusReason ::
  InstanceStatusReason
mkInstanceStatusReason =
  InstanceStatusReason' {message = Lude.Nothing}

-- | The message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrMessage :: Lens.Lens' InstanceStatusReason (Lude.Maybe Lude.Text)
isrMessage = Lens.lens (message :: InstanceStatusReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InstanceStatusReason)
{-# DEPRECATED isrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON InstanceStatusReason where
  parseJSON =
    Lude.withObject
      "InstanceStatusReason"
      (\x -> InstanceStatusReason' Lude.<$> (x Lude..:? "Message"))

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceStateFault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceStateFault
  ( SpotInstanceStateFault (..),

    -- * Smart constructor
    mkSpotInstanceStateFault,

    -- * Lenses
    sisfCode,
    sisfMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Instance state change.
--
-- /See:/ 'mkSpotInstanceStateFault' smart constructor.
data SpotInstanceStateFault = SpotInstanceStateFault'
  { code ::
      Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotInstanceStateFault' with the minimum fields required to make a request.
--
-- * 'code' - The reason code for the Spot Instance state change.
-- * 'message' - The message for the Spot Instance state change.
mkSpotInstanceStateFault ::
  SpotInstanceStateFault
mkSpotInstanceStateFault =
  SpotInstanceStateFault'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The reason code for the Spot Instance state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisfCode :: Lens.Lens' SpotInstanceStateFault (Lude.Maybe Lude.Text)
sisfCode = Lens.lens (code :: SpotInstanceStateFault -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: SpotInstanceStateFault)
{-# DEPRECATED sisfCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The message for the Spot Instance state change.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisfMessage :: Lens.Lens' SpotInstanceStateFault (Lude.Maybe Lude.Text)
sisfMessage = Lens.lens (message :: SpotInstanceStateFault -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: SpotInstanceStateFault)
{-# DEPRECATED sisfMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML SpotInstanceStateFault where
  parseXML x =
    SpotInstanceStateFault'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")

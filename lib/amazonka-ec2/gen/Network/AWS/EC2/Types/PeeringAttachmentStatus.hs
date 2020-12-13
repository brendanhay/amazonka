{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringAttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringAttachmentStatus
  ( PeeringAttachmentStatus (..),

    -- * Smart constructor
    mkPeeringAttachmentStatus,

    -- * Lenses
    pasCode,
    pasMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the transit gateway peering attachment.
--
-- /See:/ 'mkPeeringAttachmentStatus' smart constructor.
data PeeringAttachmentStatus = PeeringAttachmentStatus'
  { -- | The status code.
    code :: Lude.Maybe Lude.Text,
    -- | The status message, if applicable.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PeeringAttachmentStatus' with the minimum fields required to make a request.
--
-- * 'code' - The status code.
-- * 'message' - The status message, if applicable.
mkPeeringAttachmentStatus ::
  PeeringAttachmentStatus
mkPeeringAttachmentStatus =
  PeeringAttachmentStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The status code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasCode :: Lens.Lens' PeeringAttachmentStatus (Lude.Maybe Lude.Text)
pasCode = Lens.lens (code :: PeeringAttachmentStatus -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: PeeringAttachmentStatus)
{-# DEPRECATED pasCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The status message, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasMessage :: Lens.Lens' PeeringAttachmentStatus (Lude.Maybe Lude.Text)
pasMessage = Lens.lens (message :: PeeringAttachmentStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: PeeringAttachmentStatus)
{-# DEPRECATED pasMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML PeeringAttachmentStatus where
  parseXML x =
    PeeringAttachmentStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")

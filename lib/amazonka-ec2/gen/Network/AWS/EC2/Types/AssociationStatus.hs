{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociationStatus
  ( AssociationStatus (..),

    -- * Smart constructor
    mkAssociationStatus,

    -- * Lenses
    asCode,
    asMessage,
  )
where

import Network.AWS.EC2.Types.AssociationStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a target network association.
--
-- /See:/ 'mkAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { code ::
      Lude.Maybe AssociationStatusCode,
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

-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the target network association.
-- * 'message' - A message about the status of the target network association, if applicable.
mkAssociationStatus ::
  AssociationStatus
mkAssociationStatus =
  AssociationStatus' {code = Lude.Nothing, message = Lude.Nothing}

-- | The state of the target network association.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCode :: Lens.Lens' AssociationStatus (Lude.Maybe AssociationStatusCode)
asCode = Lens.lens (code :: AssociationStatus -> Lude.Maybe AssociationStatusCode) (\s a -> s {code = a} :: AssociationStatus)
{-# DEPRECATED asCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the target network association, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMessage :: Lens.Lens' AssociationStatus (Lude.Maybe Lude.Text)
asMessage = Lens.lens (message :: AssociationStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AssociationStatus)
{-# DEPRECATED asMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML AssociationStatus where
  parseXML x =
    AssociationStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")

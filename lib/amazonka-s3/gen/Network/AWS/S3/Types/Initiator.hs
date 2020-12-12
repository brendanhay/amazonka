{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Initiator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Initiator
  ( Initiator (..),

    -- * Smart constructor
    mkInitiator,

    -- * Lenses
    iDisplayName,
    iId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container element that identifies who initiated the multipart upload.
--
-- /See:/ 'mkInitiator' smart constructor.
data Initiator = Initiator'
  { displayName :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Initiator' with the minimum fields required to make a request.
--
-- * 'displayName' - Name of the Principal.
-- * 'id' - If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
mkInitiator ::
  Initiator
mkInitiator =
  Initiator' {displayName = Lude.Nothing, id = Lude.Nothing}

-- | Name of the Principal.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Initiator (Lude.Maybe Lude.Text)
iDisplayName = Lens.lens (displayName :: Initiator -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Initiator)
{-# DEPRECATED iDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Initiator (Lude.Maybe Lude.Text)
iId = Lens.lens (id :: Initiator -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Initiator)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML Initiator where
  parseXML x =
    Initiator'
      Lude.<$> (x Lude..@? "DisplayName") Lude.<*> (x Lude..@? "ID")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeFilter
  ( HandshakeFilter (..),

    -- * Smart constructor
    mkHandshakeFilter,

    -- * Lenses
    hfParentHandshakeId,
    hfActionType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.ActionType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the criteria that are used to select the handshakes for the operation.
--
-- /See:/ 'mkHandshakeFilter' smart constructor.
data HandshakeFilter = HandshakeFilter'
  { parentHandshakeId ::
      Lude.Maybe Lude.Text,
    actionType :: Lude.Maybe ActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HandshakeFilter' with the minimum fields required to make a request.
--
-- * 'actionType' - Specifies the type of handshake action.
--
-- If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
-- * 'parentHandshakeId' - Specifies the parent handshake. Only used for handshake types that are a child of another type.
--
-- If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ .
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
mkHandshakeFilter ::
  HandshakeFilter
mkHandshakeFilter =
  HandshakeFilter'
    { parentHandshakeId = Lude.Nothing,
      actionType = Lude.Nothing
    }

-- | Specifies the parent handshake. Only used for handshake types that are a child of another type.
--
-- If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ .
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'parentHandshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfParentHandshakeId :: Lens.Lens' HandshakeFilter (Lude.Maybe Lude.Text)
hfParentHandshakeId = Lens.lens (parentHandshakeId :: HandshakeFilter -> Lude.Maybe Lude.Text) (\s a -> s {parentHandshakeId = a} :: HandshakeFilter)
{-# DEPRECATED hfParentHandshakeId "Use generic-lens or generic-optics with 'parentHandshakeId' instead." #-}

-- | Specifies the type of handshake action.
--
-- If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfActionType :: Lens.Lens' HandshakeFilter (Lude.Maybe ActionType)
hfActionType = Lens.lens (actionType :: HandshakeFilter -> Lude.Maybe ActionType) (\s a -> s {actionType = a} :: HandshakeFilter)
{-# DEPRECATED hfActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Lude.ToJSON HandshakeFilter where
  toJSON HandshakeFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentHandshakeId" Lude..=) Lude.<$> parentHandshakeId,
            ("ActionType" Lude..=) Lude.<$> actionType
          ]
      )
